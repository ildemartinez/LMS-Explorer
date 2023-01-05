unit LMSConstsUnit;

interface

const

  WSTOKEN = 'wstoken';

  // API
  WSFUNCTION = 'wsfunction';

  CORE_COURSE = 'core_course_';
  CORE_COURSE_GET_CATEGORIES = CORE_COURSE + 'get_categories';
  CORE_COURSE_GET_COURSES = CORE_COURSE + 'get_courses';

  CORE_ENROL = 'core_enrol_';
  CORE_ENROL_GET_ENROLLED_USERS = CORE_ENROL + 'get_enrolled_users';

  // URL
  COURSE_VIEW = '/course/view.php?id=%d';
  CATEGORY_VIEW = '/course/index.php?categoryid=%d';
  USERS_VIEW = '/user/index.php?id=%d';

implementation

end.
