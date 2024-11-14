sass::sass(
    input = list(
        sass::sass_file("www/sass/variables.scss"),
        sass::sass_file("www/sass/typo.scss"),
        sass::sass_file("www/sass/hack.scss"),
        sass::sass_file("www/sass/app.scss")
    ),
    output = "www/css/app.min.css",
    options = sass::sass_options(output_style = "compressed")
)
