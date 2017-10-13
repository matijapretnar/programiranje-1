import requests
import re
import os
import csv


########################################################################
# First, let's write some functions to get the data from the web.
########################################################################

# define the URL of the main page of the bolha cats listing
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# the directory to which we save our data
cat_directory = 'my_cats'
# the filename we use to save the frontpage
frontpage_filename = "cats_fp.html"
# the filename for the CSV file for the extracted data
csv_filename = "cats.csv"

def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        print("failed to connect to url " + url)
        return
    if r.status_code == requests.codes.ok:
        return r.text
    print("failed to download url " + url)
    return


def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w') as file_out:
        file_out.write(text)
    return None


# Define a function that downloads the frontpage and saves it to a file.
def save_frontpage():
    '''Save "cats_frontpage_url" to the file "cat_directory"/"frontpage_filename"'''
    text = download_url_to_string(cats_frontpage_url)
    save_string_to_file(text, cat_directory, frontpage_filename)
    return None


########################################################################
# Now that we have some data, we can think about processing it.
########################################################################

def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.
    '''
    path = os.path.join(directory, filename)
    with open(path, 'r') as file_in:
        return file_in.read()


# Define a function that takes a webpage as a string and splits it into
# segments such that each segment corresponds to one advertisement. This
# function will use a regular expression that delimits the beginning and end of
# each ad. Return the list of strings.
# Hint: To build this reg-ex, you can use your text editor's regex search functionality.
def page_to_ads(page):
    '''Split "page" to a list of advertisement blocks.'''
    rx = re.compile(r'<div class="ad">(.*?)<div class="clear">',
                    re.DOTALL)
    ads = re.findall(rx, page)
    return ads

# Define a function that takes a string corresponding to the block of one
# advertisement and extracts from it the following data: Name, price, and
# the description as displayed on the page.
def get_dict_from_ad_block(block):
    '''Build a dictionary containing the name, description and price of an ad block.'''
    rx = re.compile(r'title="(?P<name>.*?)"'
                    r'.*?</h3>\s*(?P<description>.*?)\s*</?div'
                    r'.*?class="price">(?P<price>.*?)</div',
                    re.DOTALL)
    data = re.search(rx, block)
    ad_dict = data.groupdict()
    return ad_dict



# Write a function that reads a page from a file and returns the list of
# dictionaries containing the information for each ad on that page.
def ads_from_file(filename, directory):
    '''Parse the ads in filename/directory into a dictionary list.'''
    page = read_file_to_string(filename, directory)
    blocks = page_to_ads(page)
    ads = [get_dict_from_ad_block(block) for block in blocks]
    return ads

def ads_frontpage():
    return ads_from_file(cat_directory, frontpage_filename)


########################################################################
# We processed the data, now let's save it for later.
########################################################################

def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.

    '''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None


# Write a function that takes a list of cat advertisement dictionaries and
# writes it to a csv file.
def write_cat_ads_to_csv(ads, directory, filename):
    '''Write a CSV file containing one ad from "ads" on each row.'''
    write_csv(ads[0].keys(), ads, directory, filename)

def write_cat_csv(ads):
    '''Save "ads" to "cat_directory"/"csv_filename"'''
    write_cat_ads_to_csv(ads, cat_directory, csv_filename)
