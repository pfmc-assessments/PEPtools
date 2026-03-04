#' Add alternative text into latex
#'
#' @param dir directory where the tex file is located that will be edited.
#' Should be absolute file path or relative to current working directory.
#' @param compile Indicate whether the document should be
#' rendered after these files are changed. Default TRUE.
#' @param rename Indicate a name for the new tex file produced from this
#' function. There is no need to include ".tex" in the name. Defaults to current
#' name and overwrites the current tex file. If you provide a name, new file will
#' be dir/new_name.tex
#' @param alttext_csv Absolute path or path relative to your current working
#' directory to file containing labels and alt text
#'
#' @details This function was made to help add in
#' alternative text to LaTeX documents generated from
#' quarto. Quarto does not currently contain a way to
#' add alternative text to PDF documents, so this function
#' was developed as a work around. The addition of alternative
#' text needs to be found in either 1) the rda files produced from
#' stockplotr::exp_all_figs_tables, 2) the captions_alt_text.csv also produced from
#' the same function, or 3) a csv file with
#' columns containing "label" and "alt_text". All figure labels in the qmd files must contain a "fig-" prefix.
#' The "label" column of the csv file must remove that "fig-" prefix.
#'
#' Figures will no longer automatically be resized to the page margins unless you
#' have forced that in another way. Adding the following code to your LaTeX
#' header SHOULD work
#' `\setkeys{Gin}{width=\linewidth}`
#'
#' Debugging tip: If you get an error "Paragraph ended before
#' HyPsd@@ProtectSpacesFi was complete" along with a line number, the figure
#' near that line in the tex file does not have alt text in the csv file.
#' @author Samantha Schiano, Sophie Breitbart, Kiva Oken
#' @export

add_alttext <- function(
  dir = getwd(),
  alttext_csv = file.path(getwd(), "captions_alt_text.csv"),
  compile = TRUE,
  rename = NULL
) {
  # Read latex file
  x <- list.files(dir)[grep("skeleton.tex", list.files(dir))]
  tex_file <- readLines(file.path(dir, x))

  # Identify lines with figures
  fig_lines <- grep("\\\\includegraphics", tex_file)

  # New method: find line with label from csv then back track to lines with pandoc_bounded
  # Add alt text to custom images
  # read in alt text csv file to match with labels
  alttext <- utils::read.csv(alttext_csv)
  for (i in 1:nrow(alttext)) {
    # Find line label
    label <- glue::glue("fig-{alttext$label[i]}")
    label_line_idx <- grep(
      paste0("\\\\caption\\{\\\\label\\{", label, "\\}"),
      tex_file
    )
    if (length(label_line_idx) == 0) {
      stop(paste(
        "The following figure label does not appear in the document:",
        label
      ))
    }
    # Find which figure is right before this from fig_lines
    # only select the number closest to label_line_idx
    fig_line_idx <- max(fig_lines[fig_lines < label_line_idx])
    # Identify the line where the figure is names and loaded in
    # This is the line that will contain the alt text
    line <- tex_file[fig_line_idx]

    # Add selected alttext onto end of the line
    tex_file[fig_line_idx] <- paste(
      line,
      "{",
      alttext$alt_text[i],
      "}",
      sep = ""
    )
  }

  # Replace pandocbounded with pdftooltip so alt text can be added
  # This only works when not tagging documents
  # This is an older method of applying alt text
  tex_file[fig_lines] <- lapply(
    tex_file[fig_lines],
    function(line) {
      gsub("\\pandocbounded", "\\pdftooltip", line)
    }
  )

  # Save overwrite tex file
  write(
    unlist(tex_file),
    file = file.path(
      dir,
      ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)
    )
  )
  # utils::capture.output(cat(tex_file), file = file.path(dir, ifelse(!is.null(rename), glue::glue("{rename}.tex"), x)), append = FALSE)
  message("______Alternative text added to tex file.______")
  # Render the .tex file after edits
  if (compile) {
    message("______Compiling in progress - This can take a while...______")
    withr::with_dir(
      # changes the working directory only for rendering the tex file
      dir,
      tinytex::lualatex(file.path(ifelse(
        !is.null(rename),
        glue::glue("{rename}.tex"),
        x
      )))
    )
    message("______Compiling finished______")
  }
}
