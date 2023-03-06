function listFiles(path)
    print("path: " .. path)
    return {
        { type = "dir"
        , name = "test directory"
        , path = "test"
        }
    }
end
