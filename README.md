# job_application_shiny_project
JobVis is designed to allow job seekers to build interactive visualizations and make useful comparisons between companies and industries based on job characteristics they value the most. Through these features, our app aims to provide deeper insights on an industry and company level so that users can assess whether they are aligned with the company or industry’s requirements and if they fit in with the company/industry culture.

Motivation for this project stemmed from our observations that career portals very rarely provided a holistic view of culture, ratings, skills required (etc), at either an industry level or company level. Instead users would have to manually check each job posting to form an understanding of each characteristic. Additionally, we found that many lacked the nuances of allowing users to make tailored comparisons, and if they did it would be in a tabular format (example: Glassdoors Salary). Another major flaw was that portals did not display visualizations that allow users to keep track of their job applications and success rates. As such, JobVis addresses these pain points through graphical representations, filter functions and an improved UI layout that simplifies and adds value to the job searching experience.

To note, we believe this app is particularly important today since Covid-19 has severely impacted the job market around the world, and has left job seekers questioning which industries and companies are still hiring, what skills they should upgrade to improve chances of landing jobs, and what industry/company best suits their requirements. Moreover, we see that companies are switching to utilizing AI to select candidates, thus it has become imperative for job seekers to determine the keywords that companies are searching for in each job posting and address them in either their resumes or in their job interview. 

Job postings shown in our app are scraped from US’ Indeed.com (https://www.indeed.com/jobs?q=data%20scientist&l=all&start=0). The website shows a variety of information from the job position title to the company’s ratings and salary values. Since the information is scraped from online, we were only able to retrieve information for 70 pages (i.e. 1046 data points). Moreover, employee reviews was only extracted for Google, since . We also used “Google Skills” dataset from Kaggle as an example to show users how the application will run when specific companies are chosen for analysis and the different data visualizations.

JobVis has these features that sets us apart from conventional job portals such as Symplicity, Indeed and Glassdoors. These features directly target pain points that we found job seekers find most frustrating. Thus, our unique selling points are as listed below:
1. Radar Charts for Industry Comparison across 7 Metrics
2. Bar Charts for Happiness Ratings Comparison between Industries
3. Word Charts for Reviews Analysis
4. Bar Charts for Skills Requirements Analysis
5. Heat Maps and Chloropleth Maps for Hiring Spots Analysis
6. Sunburst Chart for Job Distribution
7. Filter Tables for Salary Comparison
8. Sankey Graph for Application Tracker
