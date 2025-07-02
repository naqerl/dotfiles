-- A place to store the arguments of the last :make command
local state = {
    last_make_args = nil
}

-- Create an augroup for our autocommands to avoid duplicates
local makex_augroup = vim.api.nvim_create_augroup("MakeXRunner", { clear = true })

-- Define the autocommand to capture the :make arguments from history
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
    pattern = "make",
    group = makex_augroup,
    callback = function()
        local last_cmd = vim.fn.histget('cmd', -1)
        if last_cmd and last_cmd:match("^make") then
            local args = last_cmd:gsub("^make%s*", "")
            state.last_make_args = args
        end
    end,
})

-- Define the function to be called by the keymap and command
local function run_saved_make()
    if state.last_make_args then
        -- If we have saved arguments, run :make with them
        vim.cmd("make " .. state.last_make_args)
    else
        -- If not, inform the user
        vim.notify("No :make arguments saved. Run :make manually first.", vim.log.levels.WARN)
    end
end

-- Map <F8> to run the saved make command
vim.keymap.set("n", "<F8>", run_saved_make, {
    noremap = true,
    silent = true,
    desc = "Execute the last :make command with its arguments"
})

-- Create the :Recompile user command
vim.api.nvim_create_user_command("Recompile", run_saved_make, {
    nargs = 0,
    desc = "Execute the last :make command with its arguments"
})
