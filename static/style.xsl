<?xml version="1.0" encoding="UTF-8"?> 
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"> 
<xsl:template match="/"> 
<html> 
    <head>
        <link rel="stylesheet" href="/static/style.css"/>
    </head>
    <body lang="en-US"> 
        <xsl:apply-templates />
    </body> 
</html> 
</xsl:template>

<xsl:template match="/meta">
    <h1><xsl:value-of select="filename" /></h1>
    <table class="property-table">
        <xsl:for-each select="property">
            <tr>
                <th><xsl:value-of select="@name" /></th>
                <td><xsl:value-of select="@value" /></td>
            </tr>
        </xsl:for-each>
    </table>
</xsl:template>

<xsl:template match="/error">
    <h1>Error</h1>
    <div class="error">
        <xsl:value-of select="//error" />
    </div>
</xsl:template>

<xsl:template match="/listing">
    <ul class="listing">
    <xsl:for-each select="directory"> 
        <li class="directory">
            <a>
                <xsl:attribute name="href">
                    <xsl:value-of select="path" />
                </xsl:attribute>
                <img src="/static/icons/folder.png" />
                <div>
                    <xsl:value-of select="name" />
                </div>
            </a>
        </li>
    </xsl:for-each>
    <xsl:for-each select="file"> 
        <li class="file">
            <a>
                <xsl:attribute name="href">
                    <xsl:value-of select="path" /><xsl:text>?t=pdf</xsl:text>
                </xsl:attribute>
                <img src="/static/icons/chart.png" />
                <div>
                    <xsl:value-of select="name" />
                </div>
            </a>
        </li>
    </xsl:for-each>
    </ul>
</xsl:template>
</xsl:stylesheet> 
