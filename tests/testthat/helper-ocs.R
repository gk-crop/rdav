mock_share_response <- function(status_code = 200) {
  resp_body <- '<?xml version="1.0"?>
<ocs>
 <meta>
  <status>ok</status>
  <statuscode>200</statuscode>
  <message>OK</message>
 </meta>
 <data>
  <element>
   <id>1081111</id>
   <share_type>3</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>1</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1636228602</stime>
   <parent/>
   <expiration/>
   <token>wjn6dK***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label></label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/a</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>1683045553</item_source>
   <file_source>1683045553</file_source>
   <file_parent>1620994286</file_parent>
   <file_target>/a</file_target>
   <item_size>0</item_size>
   <item_mtime>1676666170</item_mtime>
   <share_with>1|***</share_with>
   <share_with_displayname>(Shared Link)</share_with_displayname>
   <password>***</password>
   <send_password_by_talk></send_password_by_talk>
   <url>https://example.com/s/wjn6dK***</url>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
  <element>
   <id>1502082</id>
   <share_type>3</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>15</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1703640861</stime>
   <parent/>
   <expiration/>
   <token>3GgX8p***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label></label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/books</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>2543338818</item_source>
   <file_source>2543338818</file_source>
   <file_parent>1620994286</file_parent>
   <file_target>/books</file_target>
   <item_size>106727</item_size>
   <item_mtime>1759322495</item_mtime>
   <share_with>3|***</share_with>
   <share_with_displayname>(Shared Link)</share_with_displayname>
   <password>***</password>
   <send_password_by_talk></send_password_by_talk>
   <url>https://example.com/s/3GgX8p***</url>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
  <element>
   <id>1038527</id>
   <share_type>3</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>1</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1630149890</stime>
   <parent/>
   <expiration/>
   <token>dCopD9***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label></label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/h</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>1620994367</item_source>
   <file_source>1620994367</file_source>
   <file_parent>1620994286</file_parent>
   <file_target>/h</file_target>
   <item_size>0</item_size>
   <item_mtime>1749127658</item_mtime>
   <share_with>1|***</share_with>
   <share_with_displayname>(Shared Link)</share_with_displayname>
   <password>***</password>
   <send_password_by_talk></send_password_by_talk>
   <url>https://example.com/s/dCopD9***</url>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
  <element>
   <id>1498527</id>
   <share_type>3</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>31</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1702864315</stime>
   <parent/>
   <expiration/>
   <token>OsKtXj***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label>RW Permission</label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/hpc</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>2537637789</item_source>
   <file_source>2537637789</file_source>
   <file_parent>1620994286</file_parent>
   <file_target>/hpc</file_target>
   <item_size>4483541</item_size>
   <item_mtime>1745525146</item_mtime>
   <share_with>1|***</share_with>
   <share_with_displayname>(Shared Link)</share_with_displayname>
   <password>***</password>
   <send_password_by_talk></send_password_by_talk>
   <url>https://example.com/s/OsKtXj***</url>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
  <element>
   <id>1498530</id>
   <share_type>3</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>1</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1702866649</stime>
   <parent/>
   <expiration/>
   <token>XfPJvX***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label></label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/hpc</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>2537637789</item_source>
   <file_source>2537637789</file_source>
   <file_parent>1620994286</file_parent>
   <file_target>/hpc</file_target>
   <item_size>4483541</item_size>
   <item_mtime>1745525146</item_mtime>
   <share_with>3|***</share_with>
   <share_with_displayname>(Shared Link)</share_with_displayname>
   <password>***</password>
   <send_password_by_talk></send_password_by_talk>
   <url>https://example.com/s/XfPJvX***</url>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
  <element>
   <id>1426741</id>
   <share_type>3</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>1</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1691849649</stime>
   <parent/>
   <expiration/>
   <token>mebez2***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label></label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/l</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>2431988575</item_source>
   <file_source>2431988575</file_source>
   <file_parent>1620994286</file_parent>
   <file_target>/l</file_target>
   <item_size>0</item_size>
   <item_mtime>1705574400</item_mtime>
   <share_with>1|***</share_with>
   <share_with_displayname>(Shared Link)</share_with_displayname>
   <password>***</password>
   <send_password_by_talk></send_password_by_talk>
   <url>https://example.com/s/mebez2***</url>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
  <element>
   <id>1251937</id>
   <share_type>3</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>1</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1663695714</stime>
   <parent/>
   <expiration/>
   <token>4HvsBa***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label></label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/m</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>1964784460</item_source>
   <file_source>1964784460</file_source>
   <file_parent>1620994286</file_parent>
   <file_target>/m</file_target>
   <item_size>0</item_size>
   <item_mtime>1741220707</item_mtime>
   <share_with>1|***</share_with>
   <share_with_displayname>(Shared Link)</share_with_displayname>
   <password>***</password>
   <send_password_by_talk></send_password_by_talk>
   <url>https://example.com/s/4HvsBa***</url>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
  <element>
   <id>1616813</id>
   <share_type>3</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>15</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1722541757</stime>
   <parent/>
   <expiration/>
   <token>mNJy5g***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label></label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/ma</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>2914118015</item_source>
   <file_source>2914118015</file_source>
   <file_parent>1620994286</file_parent>
   <file_target>/ma</file_target>
   <item_size>0</item_size>
   <item_mtime>1743417257</item_mtime>
   <share_with>1|***</share_with>
   <share_with_displayname>(Shared Link)</share_with_displayname>
   <password>***</password>
   <send_password_by_talk></send_password_by_talk>
   <url>https://example.com/s/mNJy5g***</url>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
  <element>
   <id>1819214</id>
   <share_type>3</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>17</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1754253819</stime>
   <parent/>
   <expiration/>
   <token>LfiwRE***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label></label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/s</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>2296213630</item_source>
   <file_source>2296213630</file_source>
   <file_parent>1620994286</file_parent>
   <file_target>/s</file_target>
   <item_size>0</item_size>
   <item_mtime>1755160258</item_mtime>
   <share_with>3|***</share_with>
   <share_with_displayname>(Shared Link)</share_with_displayname>
   <password>***</password>
   <send_password_by_talk></send_password_by_talk>
   <url>https://example.com/s/LfiwRE***</url>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
  <element>
   <id>1876837</id>
   <share_type>4</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>17</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1761697738</stime>
   <parent/>
   <expiration/>
   <token>iCDQsZ***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label></label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/test</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>3434491981</item_source>
   <file_source>3434491981</file_source>
   <file_parent>1620994286</file_parent>
   <file_target/>
   <item_size>0</item_size>
   <item_mtime>1761216416</item_mtime>
   <share_with>guntherkrauss@gmx.net</share_with>
   <password>***</password>
   <password_expiration_time/>
   <send_password_by_talk></send_password_by_talk>
   <share_with_displayname>guntherkrauss@gmx.net</share_with_displayname>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
  <element>
   <id>1038521</id>
   <share_type>3</share_type>
   <uid_owner>jdoe@example.com</uid_owner>
   <displayname_owner>Doe, John (jdoe@example.com)</displayname_owner>
   <permissions>1</permissions>
   <can_edit>1</can_edit>
   <can_delete>1</can_delete>
   <stime>1630149345</stime>
   <parent/>
   <expiration/>
   <token>FfOw2P***</token>
   <uid_file_owner>jdoe@example.com</uid_file_owner>
   <note></note>
   <label></label>
   <displayname_file_owner>Doe, John (jdoe@example.com)</displayname_file_owner>
   <path>/exchange/v</path>
   <item_type>folder</item_type>
   <item_permissions>31</item_permissions>
   <is-mount-root></is-mount-root>
   <mount-type></mount-type>
   <mimetype>httpd/unix-directory</mimetype>
   <has_preview></has_preview>
   <storage_id>home::jdoe@example.com</storage_id>
   <storage>2404</storage>
   <item_source>1620995852</item_source>
   <file_source>1620995852</file_source>
   <file_parent>1620994286</file_parent>
   <file_target>/v</file_target>
   <item_size>0</item_size>
   <item_mtime>1707956814</item_mtime>
   <share_with>1|***</share_with>
   <share_with_displayname>(Shared Link)</share_with_displayname>
   <password>***</password>
   <send_password_by_talk></send_password_by_talk>
   <url>https://example.com/s/FfOw2P***</url>
   <mail_send>0</mail_send>
   <hide_download>0</hide_download>
   <attributes/>
  </element>
 </data>
</ocs>
'
  httr2::response(status_code = status_code,
                  headers = list("Content-Type" = "application/xml"),
                  body = charToRaw(resp_body))
}


mock_share <- function(req) {
  u <- req |>
    httr2::req_get_url() |>
    httr2::url_parse()

  p <-  u$query$path

  ids <- strsplit(u$path, "/")[[1]]
  id <- ids[length(ids)]
  if (id == "send-email") {
    id <- ids[length(ids) - 1]
  }

  if (req$method == "POST") {
    p <- httr2::req_get_body(req)$path
  }

  if (!is.null(p)) {
    if (p == "" || p == "exchange") {
      mock_share_response()
    } else {
      mock_share_response(404)
    }
  } else if (id == "10" || (id == "12" && req$method != "PUT")) {
    mock_share_response()
  } else if (id == "11") {
    mock_share_response(404)
  } else {
    mock_share_response(500)
  }
}



mock_users_response <- function(status_code = 200, empty = TRUE) {
  resp_body_found <- '<?xml version="1.0"?>
<ocs>
  <meta>
    <status>ok</status>
    <statuscode>100</statuscode>
    <message>OK</message>
    <totalitems/>
    <itemsperpage/>
  </meta>
  <data>
    <exact>
      <users/>
      <groups/>
      <remotes/>
      <remote_groups/>
      <emails/>
      <circles/>
      <rooms/>
    </exact>
    <users>
      <element>
        <label>Doe, Jack (jd007@example.com) (jack.doe@example.com)</label>
        <uuid>jd007@example.com</uuid>
        <name>Doe, Jack (jd007@example.com)</name>
        <value>
          <shareType>0</shareType>
          <shareWith>jd007@example.com</shareWith>
        </value>
        <shareWithDisplayNameUnique>jack.doe@example.com
        </shareWithDisplayNameUnique>
      </element>
    </users>
    <groups/>
    <remotes>
      <element>
        <label>
        Doe, Jan (jd5@example.com) (jd5@example.com.de@other.example.com)
        </label>
        <uuid>jd5@example.com</uuid>
        <name>Doe, Jan (jd5@example.com)</name>
        <type/>
        <value>
          <shareType>6</shareType>
          <shareWith>jd5@example.com@other.example.com</shareWith>
          <server>other.example.com</server>
        </value>
      </element>
     </remotes>
    <remote_groups/>
    <emails/>
    <lookup/>
    <circles/>
    <rooms/>
    <lookupEnabled/>
  </data>
</ocs>'

  resp_body_empty <- '<?xml version="1.0" encoding="UTF-8"?>
<ocs>
  <meta>
    <status>ok</status>
    <statuscode>100</statuscode>
    <message>OK</message>
    <totalitems/>
    <itemsperpage/>
  </meta>
  <data>
    <exact>
      <users/>
      <groups/>
      <remotes/>
      <remote_groups/>
      <emails/>
      <circles/>
      <rooms/>
    </exact>
    <users/>
    <groups/>
    <remotes/>
    <remote_groups/>
    <emails/>
    <lookup/>
    <circles/>
    <rooms/>
    <lookupEnabled/>
  </data>
</ocs>'
  resp_body <- ifelse(empty, resp_body_empty, resp_body_found)
  httr2::response(status_code = status_code,
                  headers = list("Content-Type" = "application/xml"),
                  body = charToRaw(resp_body))
}

mock_users <- function(req) {
  u <- req |>
    httr2::req_get_url() |>
    httr2::url_parse()
  s <-  u$query$search
  if (s == "Doe") {
    mock_users_response(200, FALSE)
  } else {
    mock_users_response(200, TRUE)
  }
}
