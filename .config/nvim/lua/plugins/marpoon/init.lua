local M = {}

-- Filter marks that are in the current file
local function in_current_file(marks)
    local current_file = vim.fn.expand('%:p')
    local result = {}
    for _, mark_info in ipairs(marks) do
        if vim.fn.fnamemodify(mark_info.file, ':p') == current_file then
            table.insert(result, mark_info)
        end
    end
    return result
end

-- Get all global alpha (letter) marks
local function global_alpha_marks()
    local all_marks = vim.fn.getmarklist()
    local result = {}
    for _, mark_info in ipairs(all_marks) do
        local mark = mark_info.mark:sub(2) -- skip the leading quote character
        if mark:match('%a') then
            table.insert(result, mark_info)
        end
    end
    return result
end

-- Update global marks only for current file
function M.update_global_marks()
    for _, info in ipairs(in_current_file(global_alpha_marks())) do
        local mark = info.mark:sub(2)
        vim.cmd('normal! m' .. mark)
    end
end

function M.setup()
    vim.api.nvim_create_augroup("marpoon_update_global_marks", { clear = true })
    vim.api.nvim_create_autocmd("BufLeave", {
        group = "marpoon_update_global_marks",
        pattern = "*",
        callback = function()
            M.update_global_marks()
        end,
    })
end

return M
