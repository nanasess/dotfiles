#!/usr/local/bin/perl

use strict;
use XMLRPC::Lite;
use LWP::Simple;
use Jcode;

my $rpc = XMLRPC::Lite->new;
my $body ="";
my $command = $ARGV[1];
my $sleep_time = $ARGV[2];
my $repeat_time = $ARGV[3];
$rpc->proxy('http://d.hatena.ne.jp/xmlrpc');

open BASETEXT, $ARGV[0];
while($_ = <BASETEXT>){
    chomp;
    $body = $body."$_\n";
}
close BASETEXT;

#XMLRPCを使ってキーワードを取得するためのサブルーチン

my $res = $rpc->call(
		     'hatena.setKeywordLink',
		     {
			 body => XMLRPC::Data->type('string',Jcode->new($body,'euc')->utf8),
			 score => 0,
#			     cname => ['book','movie'],
#			     a_target => '_blank',
#			     a_class => 'keyword',
		     }
		     );
if (my $fault = $res->fault){
    for (keys %{$fault}){
    warn $_."=>".$fault->{$_};
  }
} else {
  $body = $res->result;
  $body =~ s/&lt;/</ig;
  $body =~ s/&gt;/>/ig;
  $body =~ s/&quot;/"/ig;
#  print Jcode->new($body,'utf8')->euc;
}

$body = Jcode->new($body,'utf8')->euc;
my @hoge=();

while( $body =~ /(http:\/\/d.hatena.ne.jp\/[^"]+)/g )
{
    @hoge = (@hoge , $1)
}

my $num = @hoge;
my $tmpurl="";
my $targeturl="";
my @heke=();

while( $repeat_time > 0)
{
    $tmpurl = get($hoge[int(rand $num-1)]);
    @heke=();
    while( $tmpurl =~ /<a href="(\/[a-zA-Z0-9]+\/[0-9]{8})">/g ){
	@heke = (@heke , $1);
	}
    $num = @heke;
    print @heke;
    print "\n";
    
    $targeturl = $heke[int(rand $num-1)];
    print $targeturl;
    system ($command ,"http://d.hatena.ne.jp".$targeturl);
    sleep $sleep_time;
    $repeat_time = $repeat_time-1;
}
