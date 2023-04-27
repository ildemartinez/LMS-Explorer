# Introduction

LMS Explorer is a open source tool for Moodle administrators that deal with a lot of situations in the daily LMS management.

Features

- Fast locate course category and category at the LMS
- Fast jumping to course and users management pages ( user profile, course admninistration )
- Show all your Moodle courses from unique page

See it in action at https://www.st4makers.com/lms-explorer/

# Installation

Just modify the config.ini_dist to config.ini and copy in the same executable path. Configure the host, username and password and a service if you create your own Moodle API Service Access

# Running

You cant test it over your Moodle production instances but if you want to test it against a test Moodle instance you can deploy it using VirtualBox and https://bitnami.com/stack/moodle/virtual-machine

# API Support

List of webservices https://docs.moodle.org/dev/Web_service_API_functions

you can find a postman collection to test the Moodle API Service

# Configuring Moodle

You have to enable webservices and protocol to enable Moodle API so here goes some step that must be done:

![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/enable_webservices.jpg)

![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/enable_protocols.jpg)

You can build your own external service but you can start using the default installed Moodle mobile web service.

![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/enable_moodle_mobile_app.jpg)

At this point you have a default service "moodle_mobile_app" that can be used as a REST service for this application but some API calls are not enabled in this service. For advanced use of LMS-Explorer you have to create your own service and enable more powerfull entry points.

You can create you own service as follows
![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/add_new_external_service.JPG)

![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/create_external-service.JPG)

:warning: You should enable "Allow download files" in "Show more..." if you want the download course contents feature

Once created add functions and authorised users
![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/configure-external-service.JPG)

Add your user

![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/add_user.JPG)

If you use the Moodle Admin, for securtiy reasons you have to create the access token from Moodle admin pane
![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/create_admin_token.JPG)
![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/admin_token_created.JPG)
note: do not publish your token as I am doing (this is a local Moodle private instace)

And finally the exclusive functions for use in this tool<br />
![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/add_functions_to_service.JPG)

List of needed functions you need to enable in service functions:

| Functions                           | 
| ---------------------------------- | 
| core_course_get_categories |
| core_course_get_contents |
| core_course_get_courses |
| core_enrol_get_enrolled_users |
| core_group_get_course_groups |
| core_group_get_group_members |
| core_user_get_users |
| core_user_get_users_by_field |
| core_webservice_get_site_info |
| gradereport_user_get_grade_items  |

# Features

You can find a course, catogory or even a Moodle instance using the filter box

![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/find-course-category-moodle.jpg)

Just right mouse click over a course, category or Moodle instance will open that element in default browser.

![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/locate_course_in_moodle.jpg)

Double click on course opens the course form with the follow information. You can jump to your LMS with these shortcuts

![alt text](https://raw.githubusercontent.com/ildemartinez/LMS-Explorer/main/docs/screenshots/course_form.jpg)

# Other info

Some icons from http://icons8.com/free-ios-7-icons-in-vector/ http://icons8.com/
