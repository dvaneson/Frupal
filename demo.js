function api_response(args) {
    if (args == "newgame") 
        return {
            item: "None",
            energy: 100,
            position: [12,12]
        };

    return {
        item: "None",
        energy: 99,
        position: [13,12]
    };
}
