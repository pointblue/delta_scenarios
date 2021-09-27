
index = rmarkdown::render(input = 'Rmd/index.Rmd',
                          output_file = 'index.html',
                          output_dir = 'docs')

scenarios = rmarkdown::render(input = 'Rmd/draft_scenarios.Rmd',
                          output_file = 'draft_scenarios.html',
                          output_dir = 'docs')
