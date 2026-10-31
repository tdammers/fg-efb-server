local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

require 'common'
require 'navigator'

local rootURL =  'https://aip.dfs.de'

function getPDF(path)
    print(path)
    local id = string.gsub(path, '^.*/([^/]*).html$', '%1')
    local pdfURL = '/basicIFR/print/AD/' .. id .. '/dummy'
    return HTTP.download(URL.join(rootURL, pdfURL), ".pdf")
end

function followLinkByTextContent(navigator, selector, linkPattern)
    local qresult = navigator:query(selector)
    local li = nil
    for k, v in ipairs(qresult) do
        if (string.match(v.node.textContent, linkPattern) ~= nil) then
            link = v.node
            break
        end
    end
    if link == nil then
        error "Link not found"
    end
    navigator:go(link:attr('href'))
end

function listFiles(path)
    local navigator = HTTPNavigator:new(rootURL)

    if path == "" then
        navigator:go('basicIFR')
        followLinkByTextContent(navigator, 'li a', 'AD Aerodromes')
        followLinkByTextContent(navigator, 'li a', 'AD 2 Aerodromes')
        local qresult = navigator:query('a.folder-link')

        local result = {}
        for k, v in ipairs(qresult) do
            local link = v.node
            local href = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(href).path, '/')
            local name = link:query('span[lang=de]')[1].node.textContent

            table.insert(result,
                { type = "dir"
                ; name = name
                ; path = path
                }
            )
        end
        return result
    else
        navigator:go(path)
        local qresult = navigator:query('a.document-link')
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v.node
            local linkURL = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(linkURL).path, '/')
            local label = link:query('span[lang=en]')[1].node.textContent
            print(label)
            label = string.gsub(label, '^AD 2 [A-Z][A-Z][A-Z][A-Z] ', '')
            local number = string.gsub(label, '([-0-9]+).*', '%1')
            label = string.gsub(label, 'ICAO', '')
            label = string.gsub(label, 'Aircraft Parking/Docking Chart', 'APDC')
            label = string.gsub(label, 'AIRCRAFT PARKING/DOCKING CHART', 'APDC')
            label = string.gsub(label, 'Aerodrome Obstacle Chart', 'AOC')
            label = string.gsub(label, 'Precision Approach Terrain Chart', 'PATC')
            label = string.gsub(label, 'STANDARD ARRIVAL CHART %- INSTRUMENT %(STAR%)', 'STAR')
            label = string.gsub(label, 'STANDARD INSTRUMENT ARRIVAL ROUTES %(STAR%)', 'STAR ROUTES')
            label = string.gsub(label, 'INSTRUMENT APPROACH CHART', 'IAC')
            label = string.gsub(label, 'INSTRUMENT APPROACH PROCEDURE', 'APPR')
            label = string.gsub(label, 'OMNIDIRECTIONAL INSTRUMENT DEPARTURE CHART %(OID%)', 'OID')
            label = string.gsub(label, 'OMNIDIRECTIONAL INSTRUMENT DEPARTURE ROUTES %(OID%)', 'OID ROUTES')
            label = string.gsub(label, 'STANDARD DEPARTURE CHART %- INSTRUMENT %(SID%)', 'SID')
            label = string.gsub(label, 'STANDARD INSTRUMENT DEPARTURE ROUTES %(SID%)', 'SID ROUTES')
            label = string.gsub(label, '- *-', '-')
            label = string.gsub(label, '- *$', '')
            print(number, label)
            if string.sub(number, 1, 1) ~= "1" then
                table.insert(result,
                    { type = "pdf"
                    ; name = label
                    ; path = path
                    }
                )
            end
        end
        return result
    end
end
