# Introduction

LMS Explorer is a open source tool for Moodle administrators that deal with a lot of situations in the daily LMS management.

Features

- Fast locate course category and category at the LMS 

# Installation
Just modify the config.ini_dist to config.ini and copy in the same executable path. Configure the host, username and password and a service if you create your own Moodle API Service Access

# Running

You cant test it over your Moodle production instances but if you want to test it against a test Moodle instance you can deploy it using VirtualBox and https://bitnami.com/stack/moodle/virtual-machine

# API Support
List of webservices https://docs.moodle.org/dev/Web_service_API_functions

you can find a postman collection to test the Moodle API Service

# Configuring Moodle

You have to enable webservices and protocol to enable Moodle API so here goes some step that must be done

![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/screenshots/enable_webservices.JPG)
