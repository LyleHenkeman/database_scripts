<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--Update user role
SET IDENTITY_INSERT user_role ON

-- Insert user roles

INSERT INTO user_role (user_role_id,ax_user_id,role_id,lock_key,stamp) 
VALUES (1,1,1,590555066,3)