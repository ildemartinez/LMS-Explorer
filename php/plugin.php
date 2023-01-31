<?php

require_once(__DIR__ . '/config.php');
require_once($CFG->dirroot . '/webservice/lib.php');

$systemcontext = context_system::instance();

// Enable web services and REST protocol.
set_config('enablewebservices', true);

$enabledprotocols = get_config('core', 'webserviceprotocols');
if (stripos($enabledprotocols, 'rest') === false) {
    set_config('webserviceprotocols', $enabledprotocols . ',rest');
}

// Create a web service user.
$webserviceuser = $this->datagenerator->create_user([
    'username' => 'ws-externalquiz-user', 'firstname' => 'Externalquiz grades',
    'lastname' => 'User']);

// Create a web service role.
$wsroleid = create_role('WS Role for Externalquiz grades', 'ws-externalquiz-role', '');
set_role_contextlevels($wsroleid, [CONTEXT_SYSTEM]);
assign_capability('webservice/rest:use', CAP_ALLOW, $wsroleid, $systemcontext->id, true);
assign_capability('mod/externalquiz:receivegrades', CAP_ALLOW, $wsroleid, $systemcontext->id, true);

// Give the user the role.
role_assign($wsroleid, $webserviceuser->id, $systemcontext->id);

// Enable the externalquiz webservice.
$webservicemanager = new webservice();
$service = $webservicemanager->get_external_service_by_shortname('mod_externalquiz_receive_grades');
$service->enabled = true;
$webservicemanager->update_external_service($service);

// Authorise the user to use the service.
$webservicemanager->add_ws_authorised_user((object) ['externalserviceid' => $service->id,
    'userid' => $webserviceuser->id]);

// Create a token for the user.
$token = external_generate_token(EXTERNAL_TOKEN_PERMANENT, $service, $webserviceuser->id, $systemcontext);
