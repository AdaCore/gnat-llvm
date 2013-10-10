#! /bin/sh
for pattern in ali bc o s so
do
    find . -name "*.$pattern" -delete
done
