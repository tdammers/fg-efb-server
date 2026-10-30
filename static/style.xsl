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
    <div class="search-container">
        <xsl:for-each select="meta/searchPath">
            <form class="search" method="GET">
                <xsl:attribute name="action">
                    <xsl:value-of select="."/>
                </xsl:attribute>
                <input type="text" name="q"/>
                <button type="submit">Search</button>
            </form>
        </xsl:for-each>
    </div>
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
    <div class="page-nav">
        <div class="page-up">
            <xsl:if test="meta/path != ''">
                <a class="pager-up">
                    <xsl:attribute name="href">
                        /charts/api/<xsl:value-of select="meta/path" />/..
                    </xsl:attribute>
                    Up
                </a>
            </xsl:if>
        </div>
        <div class="pager">

            <xsl:choose>
                <xsl:when test="meta/page &gt; 0">
                    <a class="button previous">
                        <xsl:attribute name="href">
                            ?p=<xsl:value-of select="meta/page - 1"/>
                        </xsl:attribute>
                        &lt;
                    </a>
                </xsl:when>
                <xsl:otherwise>
                    <span class="button previous">
                        &lt;
                    </span>
                </xsl:otherwise>
            </xsl:choose>
            <span>
                <xsl:value-of select="meta/page + 1"/> / <xsl:value-of select="meta/numPages" />
            </span>
            <xsl:choose>
                <xsl:when test="meta/page &lt; meta/numPages - 1">
                    <a class="button next">
                        <xsl:attribute name="href">
                            ?p=<xsl:value-of select="meta/page + 1"/>
                        </xsl:attribute>
                        &gt;
                    </a>
                </xsl:when>
                <xsl:otherwise>
                    <span class="button next">
                        &gt;
                    </span>
                </xsl:otherwise>
            </xsl:choose>
        </div>
    </div>
</xsl:template>
</xsl:stylesheet> 
