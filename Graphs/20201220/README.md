# COVID-19 Analysis for 2020-12-13

<b>Purpose</b>

I started this project on 22 Apr 2020. The original purpose of this project was to analyze the [Johns Hopkins University COVID-19 data](https://github.com/CSSEGISandData/COVID-19) and to see if there were any correlations between opening states early (e.g. Texas on 20 Apr, etc) with an increased rate of confirmed COVID-19 cases (and subsequently, COVID-19 deaths).

It is evident that states that opened early (e.g. TX, FL, AZ, AR, etc) have had a dramatic increase in confirmed cases with a very strong positive correlation to the date of reopening.

Now that this has been confirmed/substantiated, my new focus for this project will be to track and publish state confirmed and death data in the US. The will be published on a weekly basis and kept in the Graphs folder of this repo.

The link to my main [COVID-19 repo is here](https://github.com/heyjcm/covid-19).

<b>US State Graphs Analysis</b>

On 14 December, the US began rolling out vaccines to front-line health care workers. A few of my friends have already received the Pfizer vaccine along with at least 128,000 others within the first week of the vaccine's release (a number that is definitely underreporting the true count of vaccinations given due to a variety of factors). In addition, the very recently approved vaccine by Moderna began their journey from their distribution centers to the rest of the nation.

In other, coincidental news, you'll notice something a little strange in this week's US Daily graphs. Namely, the Active Cases graph looks like there's an enormous spike on 14 Dec...which isn't normal. I did some investigating as to why this was and, for some reason, the Johns Hopkins time series data STOPPED reporting the US Recovered Cases data on 14 Dec onward (other countries seem to be intact). I'm not sure why this is, but hopefully JHU will reactivate those numbers because the Active Cases graph is derived by this formula:

Active Cases = Confirmed Cases - (Recovered Cases) - (Deaths)

Essentially, the formula is: people who got COVID (Confirmed Cases), minus the people who recovered from it, minus the people who died from it is equal to the number of people who are still dealing with it (Active Cases). Without the Recovered Cases data, the Active Cases number is impossible to compute.

Doing an eyeball check of each individual state graphs, however, gives us the intuitive sense that much of the country's cases continue to rise and, along with them, cases of death have increased in an almost lock-step fashion.

If I didn't post your favorite state, just let me know because I have them all, I just don't post them all. Or you can go to the link to my repo here and find your state: https://github.com/heyjcm/covid-19/tree/master/Graphs/20201213

Please let me know if you have any requests!

<b>US Daily Trends Analysis:</b>

Rather than providing active cases as of Sunday, 13 Dec. Since there are no more reports of recovered cases, I will forego the summary this week in hopes of finding an alternative solution soon.
