
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<h1>rgeos</h1>
<p>
The rgeos package is an R wrapper for the GEOS library. GEOS is a powerful open source geometry engine written in C++ that implements spatial functions and operators from the OpenGIS Simple Features for SQL specification. rgeos makes these tools available within R and integrates with existing spatial data tools through the sp package.
</p>

<h1>Development</h1>
<p>
This project is currently under active development; it has been released to CRAN, and may be downloaded within R using install.packages(), or from the package <a href="http://cran.r-project.org/web/packages/rgeos/index.html">CRAN page</a>. 
</p>

<p> The <strong>project summary page</strong> can be found <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
