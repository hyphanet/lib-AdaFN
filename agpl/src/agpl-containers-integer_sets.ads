with Ada.Containers.Ordered_Sets;

package Agpl.Containers.Integer_Sets is new
Ada.Containers.Ordered_Sets (Integer);

pragma Preelaborate (Agpl.Containers.Integer_Sets);
