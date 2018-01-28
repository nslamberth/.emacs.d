import argparse
import arrow
import praw
import json
import os

# TODO add ability to page through submissions
#      check the ListingGenerator class for info

homedir = os.path.expanduser("~")

with open(homedir + "/creds/reddit", "r") as f:
    creds = json.load(f)

reddit = praw.Reddit(
    client_id=creds['client_id'],
    client_secret=creds['client_secret'],
    username=creds['username'],
    password=creds['password'],
    user_agent=creds['user_agent']
)


def print_submission(submission, self_text=False):
    "Print submission as formatted string."
    s = submission
    t = arrow.get(s.created_utc).humanize()
    print(f"{s.title}")
    print(f"{s.url}")
    print(f"{s.num_comments} Comments - {s.subreddit_name_prefixed} {t}")
    print(f"{s.id}")
    print()
    if self_text:
        if hasattr(s, 'selftext'):
            if s.selftext:
                print(f"{s.selftext}")
                print(f"- {s.author.name} (selftext)")
    print()


def print_comment(comment):
    "Print comment as formatted string."
    c = comment
    t = arrow.get(c.created_utc).humanize()
    print(c.body)
    print(f"- {c.author.name} ({len(c.replies)} replies) {t}")
    print(f"{c.id}")
    print()
    print()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-r", dest="subreddit_name",
                        type=str, default="all")
    parser.add_argument("-c", dest="resource_id",
                        type=str, required=False,
                        help="Get comments for given resource id.")
    parser.add_argument("--order", dest="order",
                        type=str, default="new")
    args = parser.parse_args()

    subreddit = reddit.subreddit(args.subreddit_name)
    if args.order == "new":
        submissions = subreddit.new(limit=300)
    if args.order == "hot":
        submissions = subreddit.hot()
    if args.order == "top":
        submissions = subreddit.top()
    if args.order == "rising":
        submissions = subreddit.rising()

    if args.resource_id:
        r_id = args.resource_id
        if len(r_id) == 7:
            c = reddit.comment(r_id)
            c.refresh()
            print_comment(c)
            try:
                c.replies.replace_more()
            except:
                pass
            for r in c.replies:
                if r.body == '[removed]':
                    continue
                if r.body == '[deleted]':
                    continue
                if r.author is None:
                    continue
                if r.author.name == "AutoModerator":
                    continue
                else:
                    print_comment(r)
        else:
            s = reddit.submission(id=r_id)
            print_submission(s, self_text=True)
            s.comments.replace_more(limit=5)
            for c in s.comments:
                if c.body == '[removed]':
                    continue
                if c.body == '[deleted]':
                    continue
                if c.author is None:
                    continue
                if c.author.name == "AutoModerator":
                    continue
                else:
                    print_comment(c)
    else:
        for s in submissions:
            print_submission(s)
