
default_domain = "TORTUGA.PIRATEPRESS.ORG"
default_basedn = "dc=tortuga,dc=piratepress,dc=org"
default_shell = '/bin/bash'

addusertogroup <- function(user,group,basedn=default_basedn){
	paste(sep='', 'dn: cn=',group,',',basedn,'\n',
		'changeType: modify\n',
		'add: memberUid',
		'memberUid: ',user,'\n',
		'\n'
	)
}

adduser <- function(user,uid,gid,
	gecos=user,
	shell=default_shell,
	basedn=default_basedn,
	domain=default_domain
){
	paste(sep='','dn: uid=',user,',ou=users,',basedn,'\n',
		'objectClass: top\n',
		'objectClass: account\n',
		'objectClass: posixAccount\n',
		'cn: ',user,'\n',
		'uid: ',user,'\n',
		'uidNumber: ',uid,'\n',
		'gidNumber: ',gid,'\n',
		'homeDirectory: /home/',user,'\n',
		'loginShell: ',shell,'\n',
		'gecos: ',gecos,'\n',
		'userPassword: {SASL}',user,'@',domain,'\n',
		'\n'
	)
}

addgroup <- function(group, gid, basedn=default_basedn){
	paste(sep='','dn: cn=',group,',ou=groups,',basedn,'\n',
		'objectClass: top\n',
		'objectClass: posixGroup\n',
		'gidNumber: ',gid,'\n',
		'\n'
	)
}

addusers <- function( users, 
	startuid=2000, 
	usersgid=100, 
	basedn = default_basedn,
	domain = default_domain
){
	sapply(seq_along(users),
		function(i){
			uid = startuid - 1 + i
			adduser(users[i],uid,usersgid,
				basedn = default_basedn,
				domain = default_domain
			)
		}
	)
}
