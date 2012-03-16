-- Create syntax for TABLE 'blink'
CREATE TABLE `blink` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `timestamp` bigint(20) DEFAULT NULL,
  `strength` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `timestamp` (`timestamp`),
  KEY `strength` (`strength`)
) ENGINE=MyISAM;

-- Create syntax for TABLE 'esense'
CREATE TABLE `esense` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `timestamp` bigint(20) DEFAULT NULL,
  `name` varchar(255) DEFAULT NULL,
  `strength` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `timestamp` (`timestamp`),
  KEY `strength` (`strength`),
  KEY `name` (`name`)
) ENGINE=MyISAM;

-- Create syntax for TABLE 'event'
CREATE TABLE `event` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `timestamp` bigint(20) DEFAULT NULL,
  `name` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `timestamp` (`timestamp`),
  KEY `name` (`name`)
) ENGINE=MyISAM;

-- Create syntax for TABLE 'poorsignal'
CREATE TABLE `poorsignal` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `timestamp` bigint(255) DEFAULT NULL,
  `level` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `timestamp` (`timestamp`),
  KEY `level` (`level`)
) ENGINE=MyISAM;

