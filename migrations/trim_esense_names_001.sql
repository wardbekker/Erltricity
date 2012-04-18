-- Remove all double quotes from name
UPDATE `esense` e SET e.`name` = REPLACE(e.`name`, '"','');

