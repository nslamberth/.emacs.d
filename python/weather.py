import requests
import pickle
import arrow
import os
import pandas as pd
import seaborn as sns  # noqa
from matplotlib import pyplot as plt

base_domain = "https://www.metaweather.com/api"
nyc_woeid = 2459115


def search_for_location(query):
    url = f"{base_domain}/location/search/?query={query}"
    return requests.get(url)


def get_first_woeid(query):
    results = search_for_location(query)
    return results.json()[0]['woeid']


def get_todays_forecast(woeid):
    today = arrow.now().strftime("%m-%d-%Y")
    fname = f"{woeid}_{today}"
    if os.path.isfile(fname):
        print("loading from local cache...")
        with open(fname, "rb") as f:
            data = pickle.load(f)
    else:
        print("local cache out of date, refreshing...")
        url = f"{base_domain}/location/{woeid}"
        res = requests.get(url)
        data = res.json()
        with open(fname, "wb") as f:
            pickle.dump(data, f)
    return data


if __name__ == '__main__':
    data = get_todays_forecast(nyc_woeid)

    df = pd.DataFrame(data['consolidated_weather'])
    df['applicable_date'] = pd.to_datetime(df['applicable_date'])
    df['max_temp'] = ((df['max_temp'] * (9 / 5)) + 32)
    df['min_temp'] = ((df['min_temp'] * (9 / 5)) + 32)
    df = df.set_index('applicable_date')

    plt.close('all')
    fig, (ax1, ax2) = plt.subplots(2, 1, sharex=True)

    ax1 = (
        df[['max_temp', 'min_temp']]
        .plot(ax=ax1)
    )

    for x, y in zip(df.index, df['min_temp']):
        ax1.annotate(f"{int(y)}°F", xy=(x, y))

    for x, y in zip(df.index, df['max_temp']):
        ax1.annotate(f"{int(y)}°F", xy=(x, y))

    ax1.set_title('NYC - Five Day Forecast')

    ax2 = (
        df['humidity']
        .plot(ax=ax2)
    )
    for x, y in zip(df.index, df['humidity']):
        ax2.annotate(f"{int(y)}%", xy=(x, y))

    plt.xlabel("Date")
    plt.show()
