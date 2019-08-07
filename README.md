# lux-production-builder

Every quarter, the University of Washington's LUX Film Production Club facilitates the production of multiple short films. The leadership teams takes several hours every quarter to closely analyze anywhere from 50 to 150 Google Forms responses and manually place people into roles that we think would suit them best.

This program's intention is to eliminate hours of manual, arbitrary work by instantaneously and fairly placing people into roles based on Google Forms data. I began work on this personal project in February 2019 and completed it in August 2019, but am continuing to make improvements. We plan on implementing it during the 2019-2020 school year.<br>

<b>Here is, at its core, how the algorithm works:</b>


<b>Ask people for:</b><br>
    - Name<br>
    - Preferred production (rank their top 3)<br>
    - Preferred role (rank their top 3)<br>
    - Number of years in LUX Film Production Club<br>
    - Number of years at the University of Washington<br>
    - Whether they'd rather have the <i>role</i> they want, or the <i>production</i> they want.<br>
    - Any extra notes - availability conflicts, links to their work, etc.
    
<b>Once the spreadsheet is collected via Google Forms:</b><br>
    1. Sort responses first by "Number of years in LUX", then by "Number of years at UW"<br>
    2. Go through response list and place people on productions.<br><br>
    <b>ALGORITHM:</b>
    If they choose "role" as their #1 importance...<br>
    <i>If their 1st production choice has role #1 empty,</i> <b>place</b>.<br>
    <i>If their 2nd production choice has role #1 empty,</i> <b>place</b>.<br>
    <i>If their 3rd production choice has role #1 empty,</i> <b>place</b>.<br>
    <i>If their 1st production choice has role #2 empty,</i> <b>place</b>...etc.</i><br><br>
    If they choose "production" as their #1 importance...<br>
    <i>If their 1st production choice has role #1 empty,</i> <b>place</b>.<br>
    <i>If their 1st production choice has role #2 empty,</i> <b>place</b>.<br>
    <i>If their 1st production choice has role #3 empty,</i> <b>place</b>.<br>
    <i>If their 2nd production choice has role #1 empty,</i> <b>place</b>...etc.</i><br><br>

In summary...<br>
<b>INPUT:</b> CSV spreadsheet taken from Google Forms of quarterly LUX Film Production Club member survey responses<br>
<b>OUTPUT:</b> Several spreadsheets of role placements for every production

<b>Access the web app using this link: https://rachelkisela.shinyapps.io/lux_production_builder/</b>
