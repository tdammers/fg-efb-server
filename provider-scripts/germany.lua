local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

require 'common'
require 'navigator'

local rootURL =  'https://aip.dfs.de'

searchPath = "search"

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

function getAerodromes(navigator)
    navigator:go('/basicIFR')
    followLinkByTextContent(navigator, 'li a', 'AD Aerodromes')
    followLinkByTextContent(navigator, 'li a', 'AD 2 Aerodromes')
    local qresult = navigator:query('a.folder-link')
    local topURL = navigator.currentURL

    local result = {}
    for k, v in ipairs(qresult) do
        local link = v.node
        local href = link:attr('href')
        local name = link:query('span[lang=de]')[1].node.textContent
        local url = URL.join(topURL, href)
        navigator:go(url)
        local qresult = navigator:query('a.document-link')
        local icao = nil
        for _, vv in ipairs(qresult) do
            local label = vv.node:query('span[lang=en]')[1].node.textContent
            icao = string.gsub(label, '^AD 2 (%a%a%a%a).*$', '%1')
            if icao ~= nil and icao:len() == 4 then
                break
            end
        end
        -- print(icao, name)
        result[icao] =
            { name = name
            ; icao = icao
            ; path = url
            }
    end
    navigator:go('/basicIFR')
    return result
end

function compareAerodromes(a1, a2)
    return a1.path < a2.path
end

function compareSearchResults(a1, a2)
    return a1.score < a2.score
end

function listFiles(path, query)
    local navigator = HTTPNavigator:new(rootURL)

    local aerodromes = getAerodromes(navigator)

    if path == "" then
        local result = {}
        for icao, ad in pairs(aerodromes) do
            table.insert(result,
                { type = "dir"
                ; name = ad.icao .. " " .. ad.name
                ; path = ad.icao
                }
            )
        end
        table.sort(result, compareAerodromes)
        return result
    elseif path == "search" then
        local search = query['q']
        local result = {}
        for icao, ad in pairs(aerodromes) do
            local score = 0
            if ad.icao == search then
                score = score + 100
            end
            if ad.name == search then
                score = score + 50
            end
            if string.match(ad.icao, search) ~= fail then
                score = score + search:len() * 5
            end
            if string.match(ad.name, search) ~= fail then
                score = score + search:len()
            end
            if score > 0 then
                table.insert(result,
                    { type = "dir"
                    ; name = ad.icao .. " " .. ad.name
                    ; path = ad.icao
                    ; score = score
                    }
                )
            end
        end
        table.sort(result, compareSearchResults)
        return result
    else
        local ad = aerodromes[path]
        navigator:go(ad.path)
        local qresult = navigator:query('a.document-link')
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v.node
            local linkURL = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(linkURL).path, '/')
            local label = link:query('span[lang=en]')[1].node.textContent
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
