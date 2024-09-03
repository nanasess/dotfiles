<?php
print "<!doctype html>\n";
print "<html lang=\"ja\">\n";
print "<head>\n";
print "  <meta charset=\"UTF-8\">\n";
print "  <title>OAuth2 Authorization コードリクエスト</title>\n";
print "</head>\n";
print "<body>\n";

if ($_GET['p'] != "") {
    $url_file = "/tmp/oauth-url." . $_GET['p'];
    if (file_exists($url_file)) {
        $url = file_get_contents($url_file);
        print "<p>以下にアクセスしてください。</p>\n";
        print "<a href=\"";
        print $url;
        print "\">Authorization コードリクエスト</a>\n";
        unlink($url_file);
    } else {
        print "<p>エラー: リクエスト URL を取得できませんでした。もう一度やり直 してください。</p>\n";
    }
} else {
    print "エラー: リクエストが不正です。\n";
}

print "</body></html>\n";
