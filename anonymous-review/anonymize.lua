local public_repo = "^https://github%.com/urbanjuhyeon/tracking%-access%-change"
local anonymous_archive =
  "https://anonymous.4open.science/r/longitudinal-multimodal-access-8B61"

function Link(link)
  if link.target:match(public_repo) then
    local label = pandoc.utils.stringify(link.content)
    if label:lower():find("github.com/urbanjuhyeon", 1, true) then
      link.content = { pandoc.Str("the anonymous code archive") }
    end
    link.target = anonymous_archive
    link.title = ""
  end
  return link
end
