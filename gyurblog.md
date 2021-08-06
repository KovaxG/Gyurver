# Gyurblog Specification

Gyurblog file will have the following file format: `example.gyurblog`

## Parsing and Validation Rules

### General
- If two lines are found that start with `"title:"` or `"date:"` or so on, the values of each line will be concatenated. So if you have `"title: Hello\ntitle: World" the title will be "HelloWorld"`. This applies to Title, Date, Language, Tags and Intro.

### Title
- If there is a line that begins with `"title:"` the title will be the string from the title to the end of line. Trailing whitespace is ignored.
- If there is no line that starts with `"title:"`, the first line of the file is considered the title, until the end of line. Trailing whitespace is ignored.
- The title cannot be empty space. In this situation the file is invalid.

### Date
- If there is a line that begins with `"date:"` followed by a valid ISO date string, it will be considered the date. If the date is not valid ISO, the file is invalid. 
- If there is no line that starts with `"date:"`, then the line following the date (igoring blank lines) will be considered the date, if it is a valid ISO date. 
- If it is not a valid date, the file is invalid.

### Language
- If there is a line that begins with `"lang:"` or `"language:"` or `"languages:"` the string coming afterwards will count as languages. If string can be parsed into a list of languages. Otherwise the file is considered invalid.
- If no `"lang:"` or so on tag is found, the default language is considered to be EN

### Tags
- If there is a line that begins with `"tags:"` or `"labels:"` the string following it will count as the list of tags. If the string can be parsed into a list of tags. Otherwise the file is considered invalid.
- If no such line is found, there will be no tags in the list.

### Intro
- If there is a line surrounded by `()`, it is considered an intro. The line has to start with `(` and end with `)`.

### Reference
- If a line starts with a positive integer surrounded by square brackets, followed by at least a string, it is considered a reference. If it ends with a string surrounded by braces, the content in the braces is considered the url of the reference. A reference is only valid if the content of the blogpost contains a citation of the reference. This means that a reference is only valid, if the square bracket part of the reference appears in the sections of the blogpost.
- Otherwise the file is not considered valid.

### Section

#### Paragraph
- After all tags have been accounted for, every remaining line is considered a paragraph. Paragraphs are separated by newlines.

## Examples

### Minimal

Technically the only required fields are the title and date. All others are optional or can be omitted.

```
Minimal Example
2021-08-07
```

### Minimal Fully Featured
```
Minimal Fully Featured
2021-08-07
lang: EN
tags: example
Hello, this is my example [1].
[1] Me (myself)
```

### Generic Example
```
title: Example Title
date: 2021-08-07
lang: EN, RO, HU
tags: tag1, tag2 

(This is a short description)

This is a paragraph [1].

This is another paragraph [2].

[1] This is a reference (www.google.com)
[2] This is another reference (some book or other)
```
