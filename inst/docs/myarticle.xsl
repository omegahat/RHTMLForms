<?xml version="1.0"?>

<!-- Copyright the Omegahat Project for Statistical Computing, 2000 -->
<!-- Author: Duncan Temple Lang -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:bib="http://www.bibliography.org"
                xmlns:s="http://cm.bell-labs.com/stat/S4"
		exclude-result-prefixes="s"
               version="1.0">


<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl" />

<xsl:output method="html"
            encoding="ISO-8859-1"
            indent="yes"/>

<xsl:include href="/Users/duncan/Projects/org/omegahat/Docs/XSL/SLanguage.xsl" />
<!--  <xsl:include href="/Users/duncan/Projects/org/omegahat/Docs/XSL/Rstyle.xsl" /> -->
 <xsl:include href="/Users/duncan/Projects/org/omegahat/Docs/XSL/html.xsl" />

 <xsl:include href="/Users/duncan/Projects/org/omegahat/Docs/XSL/curl.xsl" /> 

</xsl:stylesheet>
