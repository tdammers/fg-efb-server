local function dumpXML(node, indent)
    if type(node) == "string" then
        print(indent .. "TEXT", node)
    elseif node["what"] == IDNodeElement then
        print(indent .. "ELEMENT", node["name"]["local"])
        for k, v in pairs(node["attr"]) do
            print(indent .. "\tATTR", k["local"], v)
        end
        for k, child in ipairs(node["nodes"]) do
            dumpXML(child, indent .. "\t")
        end
    else
        print(indent .. "UNKNOWN", node["what"])
    end
end

function listFiles(path)
    doc = parseHTML("<hello blah='test'><inner foo='bar'>world</inner></hello>")
    rootElem = doc["root"]
    dumpXML(rootElem, "")
    return {
        { type = "dir"
        , name = "test directory"
        , path = "test"
        }
    }
end
