CREATE TABLE `accounts` (
    `id` varchar(250) PRIMARY KEY,
    `balance` INT UNSIGNED DEFAULT 0,
    `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `modified_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;

CREATE TABLE `transactions` (
    `id` varchar(250) PRIMARY KEY,
    `from` varchar(250),
    `to` varchar(250),
    `amount` INT UNSIGNED DEFAULT 0,
    `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `modified_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;
