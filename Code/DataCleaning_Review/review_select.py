import csv 

with open('review_summary.csv') as f, open('review_select.csv', 'w') as o:
    reader = csv.reader(f)
    writer = csv.writer(o)
    writer.writerow(['date', 'business_id', 'text', 'stars', 'label'])
    for row in reader:
        if row[0][0].isalpha() or int(row[0].split()[0].split('-')[0]) < 2010:
            continue
        writer.writerow(row)