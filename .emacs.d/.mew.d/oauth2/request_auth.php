<?php

$AUTHURL = "https://login.microsoftonline.com/organizations/oauth2/v2.0/authorize";

print "<html><body>\n";

if ($_GET['client_id'] != "" && $_GET['response_type'] != "" && $_GET['scope'] != "") {
    $access_url = $AUTHURL . "?";
    foreach($_GET as $name => $value) {
        $access_url .= "$name=" . urlencode($value) . "&";
    }
    $access_url = rtrim ($access_url, "&");
    $pid = getmypid();
    print "Emacs で OAuth2 を利用する際の Authorization コードを取得します。
<br>\n";
    print "以下の URL に手元のブラウザからアクセスしてください。<br>\n";
    print "/oauth2/delegate-request.php?p=$pid";
    file_put_contents("/tmp/oauth-url." . $pid, $access_url);
} else {
    print "エラー: リクエストが不正です。\n";
}
print "</body></html>\n";
