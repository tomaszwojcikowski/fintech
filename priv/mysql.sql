CREATE TABLE `accounts` (
    `id` varchar(250) PRIMARY KEY,
    `balance` INT UNSIGNED DEFAULT 0,
    `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `modified_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;
