<?xml version="1.0" encoding="UTF-8"?> 
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"> 
<xsl:template match="/"> 
<html> 
    <head>
        <link rel="stylesheet" href="/static/style.css"/>
    </head>
    <body> 
        <xsl:if test="error">
            <h1>Error</h1>
            <div class="error">
                <xsl:value-of select="//error" />
            </div>
        </xsl:if>
        <ul class="listing">
        <xsl:for-each select="listing/directory"> 
            <li class="directory">
                <a>
                    <xsl:attribute name="href">
                        <xsl:value-of select="path" />
                    </xsl:attribute>
                    <xsl:value-of select="name" />
                </a>
            </li>
        </xsl:for-each>
        <xsl:for-each select="listing/file"> 
            <li class="file">
                <a>
                    <xsl:attribute name="href">
                        <xsl:value-of select="path" />
                    </xsl:attribute>
                    <xsl:value-of select="name" />
                </a>
            </li>
        </xsl:for-each>
        </ul>
    </body> 
</html> 
</xsl:template> 
</xsl:stylesheet> 
