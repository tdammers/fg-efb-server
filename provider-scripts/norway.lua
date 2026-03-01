local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

require 'common'
require 'navigator'

local rootURL = "https://aim-prod.avinor.no/"
local startURL = "https://aim-prod.avinor.no/en/AIP/"

function getPDF(path)
    return HTTP.download(URL.join(rootURL, path), ".pdf")
end

function listFiles(path)
    local navigator = HTTPNavigator:new(rootURL)

    if path == "" then
        local result = {}
        navigator:go(startURL)
        navigator:follow(".history_pane table a[href]", "href")
        navigator:follow("frame[name='eAISNavigationBase']", "src")
        navigator:follow("frame[name='eAISNavigation']", "src")
        local links = navigator:query("div#AD-2details a[id$='AD-2.24']")
        for k, node in ipairs(links) do
            local link = node.node
            local name = link:attr('id')
            print(name)
            name = string.match(name, '%u%u%u%u')
            local href = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(href).path, '/')
            table.insert(result,
                { type = "dir"
                ; name = name
                ; path = path
                }
            )
        end
        return result
    else
        navigator:go('/' .. path)
        local qresult = navigator:query("div[id$='-AD-2.24'] table>tbody>tr")
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v:query("a[href]")[1].node
            local labelTD = v:query("td")[1].node
            local linkURL = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(linkURL).path, '/')
            local label = labelTD.textContent
            label = string.gsub(label, utf8.char(160), ' ')
            label = string.gsub(label, 'Standard Departure Chart Instrument', 'SID')
            label = string.gsub(label, 'Standard Departure Routes Instrument', 'SID Routes')
            label = string.gsub(label, ' BASED ON GNSS', '')
            label = string.gsub(label, 'RNAV%s1%sSID', 'RNAV1')
            label = string.gsub(label, 'RNAV%s1%sSTAR', 'RNAV1')
            label = string.gsub(label, 'Visual Approach Chart%s*-%s*ICAO%s*-', 'VAC')
            label = string.gsub(label, 'Instrument approach chart', 'IAC')
            label = string.gsub(label, 'Standard Arrival Chart Instrument', 'STAR')
            label = string.gsub(label, 'Standard Arrival Routes Instrument', 'STAR Routes')
            label = string.gsub(label, 'Aerodrome Chart', 'ADC')
            label = string.gsub(label, 'Precision%s*Approach%s*Terrain%s*Chart%s*%(PATC%)', 'PATC')
            label = string.gsub(label, 'Aircraft%s*Parking%s*/%s*Docking%s*Chart', 'APDC')
            label = string.gsub(label, 'Aerodrome%s*Ground%s*Movement%s*Chart', 'AGMC')
            table.insert(result,
                { type = "pdf"
                ; name = label
                ; path = path
                }
            )
        end
        return result
    end
end
