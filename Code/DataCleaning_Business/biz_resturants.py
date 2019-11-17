import csv
import json
from collections import defaultdict

with open('category_biz.csv') as f:
    reader = csv.reader(f)
    restaurants_id = set()
    for row in reader:
        if row[0] == 'Restaurants':
            restaurants_id.add(row[1])
    f.close()

# print(restaurants_id)

id_features = defaultdict(lambda: defaultdict(str))
features = ["BusinessParking", "RestaurantsDelivery", "RestaurantsReservations", 
            "NoiseLevel", "RestaurantsTakeOut", "RestaurantsPriceRange2", "WiFi"]
head = ['business_id', 'stars'] + features
with open('business.json') as f:
    iter_f = iter(f)
    line = f.readline()
    for line in iter_f:
        d = json.loads(line)
        id = d['business_id']
        attributes = d['attributes']
        if id not in restaurants_id:
            continue
        id_features[id]['stars'] = d['stars']
        for feature in features:
            if attributes and feature in attributes:
                if feature != "BusinessParking":
                    id_features[id][feature] = attributes[feature].strip('u').strip('\'')
                else:
                    info = attributes[feature]
                    for c in '\'\".{}()[]:;,.!?':
                        info = info.replace(c, ' ')
                    lst = info.split()
                    id_features[id][feature] = str('True' in lst)

    f.close()
# print(id_features)


with open('biz_resturants.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerow(head)
    for id in id_features:
        writer.writerow([id, id_features[id]['stars']]+[id_features[id][feature] for feature in features])

