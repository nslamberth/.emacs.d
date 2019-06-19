import requests
import pickle
import arrow
import os
import pandas as pd
import seaborn as sns
from matplotlib import pyplot as plt
from matplotlib import dates as mdates

sns.set()

base_domain = "https://www.metaweather.com/api"
nyc_woeid = 2459115
homedir = os.path.expanduser("~")
cache = os.path.join(homedir, ".emacs.d", "python", "weather_cache")

if not os.path.isdir(cache):
    os.makedirs(cache)

def search_for_location(query):
    url = f"{base_domain}/location/search/?query={query}"
    return requests.get(url)


def get_first_woeid(query):
    results = search_for_location(query)
    return results.json()[0]['woeid']

def get_todays_forecast(woeid):
    today = arrow.now().strftime("%m-%d-%Y")
    fname = os.path.join(cache,f"{woeid}_{today}")
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


def plot_forecast(df):
    plt.close('all')
    fig, (ax1, ax2) = plt.subplots(2, 1, sharex=True)

    ax1 = (
        df[['max_temp', 'min_temp']]
        .plot(ax=ax1)
    )

    for x, y in zip(df.index, df['min_temp']):
        ax1.annotate(f"{int(y)}F", xy=(x, y))

    for x, y in zip(df.index, df['max_temp']):
        ax1.annotate(f"{int(y)}F", xy=(x, y))

    ax1.set_title('NYC - Five Day Forecast')

    ax2 = (
        df['humidity']
        .plot(ax=ax2)
    )
    for x, y in zip(df.index, df['humidity']):
        ax2.annotate(f"{int(y)}%", xy=(x, y))

    fdates = pd.Series(df.index).dt.strftime("%a, %b %d")
    states = list(df['weather_state_name'])

    xlabels = ["\n".join((fd, s)) for fd, s in list(zip(fdates, states))]

    days = mdates.DayLocator()

    ax2.minorticks_off()
    ax2.xaxis.set_major_locator(days)
    ax2.set_xticklabels(xlabels)

    plt.xlabel("Date")
    plt.show()


if __name__ == '__main__':
    data = get_todays_forecast(nyc_woeid)

    df = pd.DataFrame(data['consolidated_weather'])
    df['applicable_date'] = pd.to_datetime(df['applicable_date'])
    df['max_temp'] = ((df['max_temp'] * (9 / 5)) + 32)
    df['min_temp'] = ((df['min_temp'] * (9 / 5)) + 32)
    df = df.set_index('applicable_date')

    plot_forecast(df)
