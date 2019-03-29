Structure:
 - static assets are in their own `static` branch. I expect that one to be updated very seldomly
 - projects are in the `projects` branch. Since they are just short summaries of a project that just
     link to buildup stories etc they are also not updated regularly
 - stories and posts are in the `posts` branch. They are updated very(TM) regularly (or well at
     least the most often)

Regarding stories: Just have directories under the top level. Each directory contains a 'index.md'
that contains the overview of the story.
E.g: For the story "GameCM" there is a file "GameCM/index.md" that gives an overview over the story
"GameCM" for completely new readers that may be dumped in the middle of the story. This file is
linked to from each chapter of the story.

Each chapter of a story is a separate file in the story's directory.
On each chapter hibachi inserts a navigational bar on the top and the bottom linking to
 - The first chapter (unless the current chapter is the first one)
 - The previous chapter (unless the current chapter is the current one)
 - The overview
 - The next chapter (unless the current chapter is the last one)
 - The last chapter (unless the current chapter is the last one)
