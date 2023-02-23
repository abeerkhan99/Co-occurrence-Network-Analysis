# About This
This is part of the second assignment of the CS/SDP 361/352 Social Network Analysis Fall 2022 course that was offered at Habib University. 

## Tasks

    1. Extract news articles of different terms but for the same context
    2. Create a word co-occurrence network for each term
    3. Analyze these networks to identify how similar or different these networks


### Data Extraction
Use The Guardian API to extract the news headline and content of articles for the following terms:

    a) Terror
    b) Floods
Retrieve only those articles also containing the word “Pakistan”.

### Network Construction
Create two a network for each term. In the co-occurrence network, each word is a node and an edge represents two words that have appeared in the same article/title of the article. Perform necessary data cleaning techniques such as removing stop words and stemming.

### Network Analysis:
Perform a comparative analysis of networks of both terms to analyze which kind of language of used in different kinds of crises. 

The analysis was framed around the following question:

    Whether the language used in different kinds of crisis is similar or different?
