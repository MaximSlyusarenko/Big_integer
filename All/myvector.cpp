#include <cstring>
#include <stdexcept>
#include <iosfwd>
#include <cmath>
#include <algorithm>
#include <vector>
#include <string>
#include <iostream>
#include <cstdio>

using namespace std;

struct storage
{
	vector <int> vect;
	int number; //сколько на него ссылается
};

struct myvector
{
	bool is_vector;
	union
	{
        int integer; //small object
        storage *str; 
    };

	myvector()
	{
		is_vector = false;
		integer = -1;
	}

	~myvector()
	{
		if (is_vector)
		{
			if (str -> number > 1)
				str -> number--;
			else
				delete str;
		}
		integer = -1;
	}

    void copy(myvector& a)
    {
    	if (a.is_vector)
    	{
    		if (a.str -> number > 1)
    		{
    			storage *tmp = a.str;
    			tmp -> number--;
    			a.str = new storage;
    			a.str -> number = 1;
    			for (int i = 0; i < tmp -> vect.size(); i++)
    			{
    				a.str -> vect.push_back(tmp -> vect[i]);
    			}
    		}
    	}
    }

	void push_back(int p)
	{
		if (this -> is_vector)
		{
			copy(*this);
			this -> str -> vect.push_back(p);
		}
		else
		{
			if (this -> integer != -1)
			{
				int t = this -> integer;
				this -> str = new storage;
				this -> str -> vect.push_back(t);
				this -> str -> vect.push_back(p);
				this -> str -> number = 1;
				this -> is_vector = true;
			}
			else
			{
				this -> integer = p;
				this -> is_vector = false;
			}
		}
	}

	void pop_back()
	{
		if (this -> is_vector)
		{
			copy(*this);
			this -> str -> vect.pop_back();
			if (this -> str -> vect.size() == 1)
			{
				this -> is_vector = false;
				int t = str -> vect[0];
				delete this -> str;
				this -> integer = t;
			}
		}
		else
		{
			this -> integer = -1;
		}
	}

	int back() const
	{
		if (this -> is_vector)
		{
			return this -> str -> vect.back();
		}
		else
		{
			return this -> integer;
		}
	}

	int size() const
	{
		if (this -> is_vector)
		{
			return this -> str -> vect.size();
		}
		else
		{
			if (this -> integer != -1)
				return 1;
			else
				return 0;
		}
	}

	int& operator [] (int a)
	{
		copy(*this);
		if (this -> is_vector)
		{
			return str -> vect[a];
		}
		else
		{
			return this -> integer;
		}
	}

	int operator [] (int a) const
	{
		if (this -> is_vector)
		{
			return str -> vect[a];
		}
		else
		{
			return this -> integer;
		}
	}

	void resize(int size)
	{
		if (!this -> is_vector && size == 1)
		{
			this -> integer = 0;
			this -> is_vector = false;
			return;
		}
		if (this -> is_vector && size == 1)
		{
            copy(*this);
            delete this -> str;
            this -> integer = 0;
            this -> is_vector = false;
            return;
		}
		this -> str = new storage;
		this -> str -> number = 1;
		this -> is_vector = true;
		this -> str -> vect.resize(size);
	}

	myvector& operator = (myvector const& other)
	{
		if (is_vector)
		{
			if (str -> number > 1)
				str -> number--;
			else
				delete str;
		}
		this -> is_vector = other.is_vector;
		if (!other.is_vector)
		{
            this -> integer = other.integer;
        }
		if (other.is_vector)
		{
			other.str -> number++;
			this -> str = other.str;
		}
		return *this;
	}

	myvector(myvector const& other)
	{
        if (!other.is_vector)
        {
            integer = other.integer;
        }
        is_vector = other.is_vector;
		if (other.is_vector)
		{
			other.str -> number++;
			str = other.str;
		}
	}
};
