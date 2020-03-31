get_confirmation <- function(msg, default_yes=TRUE)
{
    non_default <- if(default_yes) "n" else "y"
    resp <- readline(msg)
    blank <- nchar(resp) == 0
    yes <- if(default_yes)
        !blank && substr(resp, 1, 1) != "n"
    else !blank && substr(resp, 1, 1) == "y"
    if(default_yes)
        blank || yes
    else !blank && yes
}
