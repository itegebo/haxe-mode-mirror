<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
  <head>
    <title>{{name}}</title>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <meta name="language" content="en" />
    
    <script src="js/swfobject.js" type="text/javascript"></script>
    <script type="text/javascript">
      var flashvars = {};
      var params = {
      menu: "false",
      scale: "noScale",
      allowFullscreen: "true",
      allowScriptAccess: "always",
      bgcolor: "#FFFFFF"
      };
      var attributes = { id:"{{name}}" };
      swfobject.embedSWF("{{name}}.swf", "altContent", "100%", "100%",
      "{{major-version}}.{{minor-version}}",
      "expressInstall.swf", flashvars, params, attributes);
    </script>
    <style type="text/css">
      html, body { height:100%; }
      body { margin:0; }
    </style>
  </head>
  <body>
    <div id="altContent">
      <h1>{{name}}</h1>
      <p>Alternative content</p>
      <p><a href="http://www.adobe.com/go/getflashplayer">
	  You need Flash player version {{major-version}}.{{minor-version}}
	  to view this content.</a></p>
    </div>
  </body>
</html>
