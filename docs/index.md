---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: default
showtoc: true
showcarousels: true
carousels:
    - images: 
        - image: assets/images/HomePage.png
        - image: assets/images/SDGByYear2020.png
        - image: assets/images/SDGOrNotPie.png
        - image: assets/images/TopAuthorsSDG14.png
        - image: assets/images/TopDeptSDG7.png
        - image: assets/images/ByDept.png
        - image: assets/images/AuthorSDG.png

         
---
# IN PROGRESS
# Motivation
![Assignment Earth Research](assets/images/Asgmt_Earth_Research.png){: width="250"}![UN's 17 SDGs](assets/images/UN_SDGs.jpg){: width="250"}

AC-9 of USC's STARS report


# Problem
1. How to classify publications into 17 SDGs
    a. ML model
2. How to display results
    a. Dashboard

# Data
1. Curated list of publications with assigned primary and secondary SDG
2. Downloaded 2020-2023 USC affiliated publications from Scopus  
    a. using Elsevier’s 2022 search queries for SDG 1 to 16  
    b. SDG 0 is all the USC publications not in SDG 1 to 16

# Methods
1. Using Aurora’s mbert? model  
    a. Gives probabilities of being in SDG 1 to 16 for each publication’s abstract + title?
2. Creating a dashboard using RShiny  
    b. Find USC authors in each publication by parsing the data, using Scopus API, web scraping USC faculty directory

# Results

1. Using Aurora’s model  
    a. 54%? Primary SDG accuracy
2. RShiny Dashboard
{% include inlinecarousel.html %}

# Discussion

- What we did
    - Run Aurora’s ML model to categorize publications
    - Create RShiny Dashboard
- Results Imply
    - Using Elsevier search queries  
        - there are alot of SDG 3 USC publications
        - Half the USC publications are not related to any SDG
        - Compare with ML output
- Improvements
    - Missing SDG 0 USC authors + author info
    - Use ML output in RShiny Dashboard
    - For each publication have a treemap/mosaic graph displaying probabilities
    - Improve performance of RShiny Dashboard 
        - a large number of dropdown options - use server side selectize


# What we learned

Learned to use tools? R + RShiny, git
