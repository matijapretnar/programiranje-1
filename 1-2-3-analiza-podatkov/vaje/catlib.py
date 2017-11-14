import requests
import re
import os
import csv


########################################################################
# First, let's write some functions to get the data from the web.
########################################################################

# define the URL of the main page of the bolha cats listing
cats_frontpage_url = "TODO"
# the directory to which we save our data
cat_directory = "TODO"
# the filename we use to save the frontpage
frontpage_filename = "TODO"
# the filename for the CSV file for the extracted data
csv_filename = "TODO"

def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        # some code here that may raise an exception
        TODO
        # some more code that won't be run if the exception occured
    except TODO:
        # some error handling / recovery code here
        # we may just display an informative message and quit
        TODO
    # continue with the non-exceptional code
    TODO

def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding = 'utf-8') as file_out:
        file_out.write(text)
    return None


# Define a function that downloads the frontpage and saves it to a file.
def undefined( TODO ):
    '''TODO'''
    TODO



########################################################################
# Now that we have some data, we can think about processing it.
########################################################################

def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.
    '''
    path = os.path.join( TODO )
    with open(path, 'r') as file_in:
        TODO


# Define a function that takes a webpage as a string and splits it into
# segments such that each segment corresponds to one advertisement. This
# function will use a regular expression that delimits the beginning and end of
# each ad. Return the list of strings.
# Hint: To build this reg-ex, you can use your text editor's regex search functionality.
def undefined( TODO ):
    '''TODO'''
    TODO


# Define a function that takes a string corresponding to the block of one
# advertisement and extracts from it the following data: Name, price, and
# the description as displayed on the page.
def undefined(block):
    '''TODO'''
    rx_price = re.compile(r'BEGINNING OF PRICE     CAPTURE THE PRICE       END OF PRICE', re.DOTALL)
    data_price = re.search(rx, block)
    # EXTRACT MORE DATA
    TODO
    # BUILD A DICTIONARY
    # documentation: https://docs.python.org/3/library/re.html (linked on učilnica)
    return ad_dict


# Write a function that reads a page from a file and returns the list of
# dictionaries containing the information for each ad on that page.
def undefined( TODO ):
    '''TODO'''
    TODO



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


# Write a function that takes a non-empty list of cat advertisement
# dictionaries and writes it to a csv file. The fieldnames can be read off the
# dictionary.
def undefined( TODO ):
    '''TODO'''
    TODO
