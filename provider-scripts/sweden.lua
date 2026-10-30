local XML = require 'xml'
local JSON = require 'json'
local HTTP = require 'http'
local URL = require 'url'

require 'common'
require 'navigator'

local rootURL  = "https://aro.lfv.se/"
local eaipBase = "/content/eaip/"

searchPath = "search"

function getAMDT(navigator)
    navigator:go(eaipBase .. "default_offline.html")
    local link = navigator:query("a[href*='index-v2.html']")[1].node
    local href = link:attr('href')
    local amdt = string.gsub(href, "index%-v2.html", "")
    if amdt == nil then
        error("Could not extract AMDT folder from: " .. href)
    end
    return amdt
end

-- Discover the current AIRAC AMDT's v2 base URL from the history page.
-- Returns e.g. "https://aro.lfv.se/content/eaip/AIRAC AIP AMDT 6-2026_2026_10_29/v2/"
function getDatasource(navigator, amdt)
    navigator:go(eaipBase .. amdt .. "/v2/js/datasource.js")
    local doc, _ = navigator:fetch("raw")
    return extractJSON(doc)
end

-- Strip the JS variable declaration wrapper and return just the JSON object.
-- Input: "const DATASOURCE = {...};"
function extractJSON(body)
    local start, _ = string.find(body, '{', 1, true)
    local result = string.sub(body, start)
    result = string.sub(result, 1, string.len(result) - 1)
    return JSON.parse(result)
end

function getMenu (data)
    return data.tabs[1].contents["en-GB"].menu[3]
end

function getAerodromes (menu)
    local result = {}
    for _, v in ipairs(menu.children) do
        if v.id == "AD 2en-GB" then
            for _, vv in ipairs(v.children) do
                local icao = string.gsub(vv.id, 'AD 2 (%w+) .*', '%1')
                local name = string.gsub(vv.title, 'AD 2 %w+ (.*)', '%1')
                if name == nil then
                    name = icao
                end
                table.insert(result,
                    { id = vv.id
                    , title = vv.title
                    , href = vv.href
                    , icao = icao
                    , name = name
                    })
            end
        end
    end
    return result
end

function getPDF(path)
    local navigator = HTTPNavigator:new(rootURL)
    local amdt = getAMDT(navigator)
    local filePath = string.gsub(path, "files/(.*)", "%1")
    local url = URL.join(rootURL .. eaipBase .. amdt .. "/", "documents/Root/SWEDEN/Charts/" .. filePath)
    return HTTP.download(url, ".pdf")
end

function listFiles(path, query)
    local navigator = HTTPNavigator:new(rootURL)
    local amdt = getAMDT(navigator)
    local data = getDatasource(navigator, amdt)
    local menu = getMenu(data)

    if path == "" or path == "search" then
        local aerodromes = getAerodromes(menu)
        local search = (path == "search") and query['q'] or nil
        local result = {}
        for _, ad in ipairs(aerodromes) do
            if search == nil or string.match(ad.id .. " " .. ad.name, search) then
                table.insert(result,
                    { type = "dir"
                    ; name = ad.icao .. " " .. ad.name
                    ; path = ad.icao
                    }
                )
            end
        end
        return result

    else
        local result = {}
        -- path is a 4-letter ICAO code
        local aerodromes = getAerodromes(menu)
        local href = nil
        for _, ad in ipairs(aerodromes) do
            if ad.icao == path then
                href = ad.href
            end
        end
        if href ~= nil then
            navigator:go(eaipBase .. amdt .. "/eAIP/" .. href)
            local items = navigator:query('a.ulink')
            for _, item in pairs(items) do
                local href = item.node:attr('href')
                local name = string.gsub(href, ".*/%u%u%u%u ([A-Za-z0-9_() ]*).pdf", "%1")
                local path = "files/" .. string.gsub(href, "../documents/Root/SWEDEN/Charts/", "")
                table.insert(result,
                    { type = "pdf"
                    ; name = name
                    ; path = path
                    }
                )
            end
        end
        return result
    end
end
