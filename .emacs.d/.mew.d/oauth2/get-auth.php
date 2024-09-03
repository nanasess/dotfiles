<?php
$AUTHURL = "https://login.microsoftonline.com/organizations/oauth2/v2.0/authoriz";

print "<html><body>\n";

if ($_GET['error'] == '') {
    $code = $_GET['code'];
    if ($code == "" ) {
        print "エラー: Authorization コードを取得できませんでした。\n";
    } else {
        print "次の Authorization コードを Emacs にコピーしてください。<br>\n";

        print '<input id="copyTarget" type="text" size="120" value="';
        print $code;
        print '" readonly>';
        print '<button onclick="copyToClipboard()">クリップボードにコピー</button>';
        print "\n";
    }
} else {
    print "エラー: 認証できませんでした。\n";
}
print "</body></html>\n";
?>

<script type="text/javascript">

function copyToClipboard() {
    var copyTarget = document.getElementById("copyTarget");
    copyTarget.select();
    document.execCommand("Copy");
    alert("コピーしました: " + copyTarget.value);
}
</script>
