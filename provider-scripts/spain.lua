local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

require 'common'
require 'navigator'

local rootURL =  "https://aip.enaire.es"
local startURL = "/AIP/AIP-en.html"

function getPDF(path)
    return HTTP.download(URL.join(rootURL, path), ".pdf")
end

function listFiles(path)
    local navigator = HTTPNavigator:new(rootURL)

    if path == "" then
        navigator:go(startURL)

        -- linkRows <- rp.dom.query("table.enlaces tr:has(td.id)");
        local qresult = navigator:query("table.enlaces tr:has(td.id)")

        -- filter((x) -> match(/^[A-Z]{4}$/, x.path),
        -- map((linkRow) -> do {
        --   id <- linkRow.query("td.id")[0].text;
        --   desc <- linkRow.query("td.desc")[0].text;
        --   {
        --     "type": "dir",
        --     "name": id ~ " - " ~ replace("/", " / ", desc),
        --     "path": id
        --   }
        -- }, linkRows))

        local result = {}
        for k, v in ipairs(qresult) do
            local id = v:query("td.id")[1].node.textContent
            local desc = v:query("td.desc")[1].node.textContent

            if string.match(id, "^[A-Z][A-Z][A-Z][A-Z]") ~= fail then
                table.insert(result,
                    { type = "dir"
                    ; name = id .. " - " .. string.gsub(desc, "/", " / ")
                    ; path = id
                    }
                )
            end
        end
        return result
    else
        print(path)
        navigator:go(startURL)
        -- linkRows <- rp.dom.query("table.enlaces tr:has(td.iconos):has(a[href$='.pdf'])");
        -- filter((x) -> x != null,
        --   map((linkRow) -> do {
        --       link <- linkRow.query("a[href$='.pdf']")[0];
        --       attr <- link.attr("href")[0];
        --       apname <- replace(/^contenido_AIP\/AD\/AD2\/([A-Z]{4})\/.*$/, "$1",
        --                 attr);
        --       case (null) {
        --           _ | (apname == pathStr) -> do {
        --               id <- linkRow.query("td.id")[0].text;
        --               desc <- linkRow.query("td.desc")[0].text;
        --               {
        --                 "type": "pdf",
        --                 "name": desc,
        --                 "path": URL.parse(attr)
        --               }
        --           };
        --           _ -> null
        --       }
        --   },
        --   linkRows))
        local qresult = navigator:query("table.enlaces tr:has(td.iconos):has(a[href$='.pdf'])")
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v:query("a[href$='pdf']")[1].node
            local attr = link:attr('href')
            local apmatch = string.match(attr, "^contenido_AIP/AD/AD2/" .. path .. "/")
            if apmatch ~= fail then
                local desc = v:query("td.desc")[1].node.textContent
                local path = intercalate((URL.parse(navigator.currentURL) .. URL.parse(attr)).path, '/')
                print(v.node)
                print(path)

                table.insert(result,
                    { type = "pdf"
                    ; name = desc
                    ; path = path
                    }
                )
            end
        end
        return result
    end
end


