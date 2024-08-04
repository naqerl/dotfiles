return {
  "ahmedkhalf/project.nvim",
  enabled = false,
  opts = {
    manual_mode = true,
  },
  event = "VeryLazy",
  config = function(_, opts)
    require("project_nvim").setup(opts)
  end,
}
