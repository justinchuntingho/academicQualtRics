# academicQualtRics

![status](https://img.shields.io/badge/status-pre_release-yellow)
![GitHub Repo stars](https://img.shields.io/github/stars/justinchuntingho/academicqualtrics)
![GitHub Repo stars](https://img.shields.io/github/forks/justinchuntingho/academicqualtrics)
![GitHub Repo stars](https://img.shields.io/github/watchers/justinchuntingho/academicqualtrics)


Repo containing code to for R package <tt>academicQualtRics</tt> to access Qualtrics API endpoint for academic research.

To cite package ‘academicqualtrics’ in publications use:
  - Ho, Justin Chun-ting. (2025). academicqualtrics: an R package to access Qualtrics API endpoint. https://github.com/justinchuntingho/academicqualtrics

## Introduction
Crowd annotation has been increasingly used within social sciences and beyond, however it often requires researchers to set up the platform for crowdcoders to conduct their annotation. Currently, a common workflow is to setup the annotation task in the form of a Qualtrics survey and forward the link to annotators recruited through panel company, for example Prolifics. However, setting up the survey on Qualtrics is not always easy, especially when you have a large collection of data. This package automates the process of uploading texts, creating questions, and adding a randomiser block in the survey flow.

## Installation

You can install the package with:
``` r
devtools::install_github("justinchuntingho/academicqualtrics")
```
## Getting your Qualtrics credentials
There are two important credentials you need to access the Qualtrics API. These are your Datacenter ID and API token. They can be checked in your Account Settings
For further information, check the [Qualtrics API documentation](https://api.qualtrics.com/). Additionally, before uploading your texts and questions, you need to create a survey and obtain the survey ID. The easiest to check is to go to the survey edit page, and check the URL, it should look something like this: `https://uva.eu.qualtrics.com/survey-builder/SV_9SwnPtsdgsdgjhY/edit`. The string starts with `SV_` is the survey ID (in this case, `SV_9SwnPtsdgsdgjhY`).

## Workflow
Once you have your Qualtrics credentials ready, add texts to Qualtrics. The function currently accepts a character vector of texts to be annotated.

The following codes will automatically create survey blocks for each text:
``` r
texts <- c("In a hole in the ground there lived a hobbit.",
            "It was a hobbit-hole, and that means comfort.")
blockids <- add_texts(texts, "fra1", "SV_S3A96bzOnfyKMEDCiKhw", "ZAhIjt6CkPO5FyczlRhJ")
```

After that, you can add then questions and answer. This step can repeat as many times as required:
``` r
question <- c("What does a Hobbit hole look like?")
answers <- c("nasty", "dirty", "comfortable")
add_questions(blockids,
              question,
              answers,
              "fra1",
              "SV_S3A96bzOnfyKMEDCiKhw",
              "ZAhIjt6CkPO5FyczlRhJ")
```

Finally, a randomiser block can be added. This will randomise the distribution of texts to annotators:
``` r
create_block_randomizer(blockids,
                        "fra1",
                        "SV_S3A96bzOnfyKMEDCiKhw",
                        "ZAhIjt6CkPO5FyczlRhJ")
```

