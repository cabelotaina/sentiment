import pprint
import treetaggerwrapper

tagger = treetaggerwrapper.TreeTagger(TAGLANG='en')
tags = tagger.tag_text("This is a very short text to tag.")

# install https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/



pprint.pprint(tags)
['This\tDT\tthis',
 'is\tVBZ\tbe',
 'a\tDT\ta',
 'very\tRB\tvery',
 'short\tJJ\tshort',
 'text\tNN\ttext',
 'to\tTO\tto',
 'tag\tVV\ttag',
 '.\tSENT\t.']