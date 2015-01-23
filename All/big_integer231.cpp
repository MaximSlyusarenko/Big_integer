#include "big_integer.h"

#include <cstring>
#include <stdexcept>
#include <iostream>
#include <cstdio>
#include <vector>
#include <cmath>
#include <algorithm>
#include <string>

#define base 2147483648

using namespace std;

big_integer::big_integer()
{
	koef.push_back(0);
	sign = false;
}

big_integer::big_integer(big_integer const& other)
{
	sign = other.sign;
	koef = other.koef;
}

big_integer::big_integer(int a)
{
	if (a < 0)
	{
		sign = true;
	}
	else
	{
		sign = false;
	}
	a = abs(a);
	if (a == base)
	{
		koef.push_back(0);
		koef.push_back(1);
		return;
	}
	koef.push_back(a);
}

big_integer::big_integer(std::string const& s)
{
	sign = false;
	int neg = (s[0] == '-' ? 1 : 0);
	koef.push_back(0);
	for (int i = neg; i < s.size(); i++)
	{
		*this *= 10;
		*this += (int(s[i]) - int('0'));
	}
	while (this -> koef.size() > 1 && this -> koef.back() == 0)
	{
		this -> koef.pop_back();
	}
	if (*this == 0)
		sign = false;
	else if (neg)
		sign = true;
	else
		sign = false;
}

big_integer::~big_integer()
{
	sign = false;
    int tmp = koef.size();
    for (int i = 0; i < tmp; i++)
    	koef.pop_back();
}

big_integer& big_integer::operator=(big_integer const& other)
{
	if (*this == other)
        return *this;
	this -> sign = other.sign;
	this -> koef = other.koef;
	return *this;
}

big_integer& big_integer::operator+=(big_integer const& rhs)
{               	
	big_integer result;
	result.koef.pop_back();
	if (this -> sign == rhs.sign)
	{
		result.sign = this -> sign;
		long long p = 0;
		int n = this -> koef.size();
		int m = rhs.koef.size();
		for (int i = 0; i < min(n, m); i++)
		{
			long long k = (long long) this -> koef[i] + (long long) rhs.koef[i] + p;
			p = 0;
			if (k >= base)
			{
				p = 1;
				k -= base;
			}
			result.koef.push_back(k);
		}
		if (n == m && p != 0)
		{
			result.koef.push_back(1);
		}
		else if (n != m)
		{
			for (int i = min(n, m); i < max(n, m); i++)
			{
				if (n > m)
				{
					long long k = (long long) this -> koef[i] + p;
					p = 0;
					if (k >= base)
					{
						p = 1;
						k -= base;
					}
					result.koef.push_back(k);
				}
				else
				{
					long long k = (long long) rhs.koef[i] + p;
					p = 0;
					if (k >= base)
					{
						p = 1;
						k -= base;
					}
					result.koef.push_back(k);
				}
			}
			if (p != 0)
			{
				result.koef.push_back(1);
			}
		}
		*this = result;
		return *this;
	}
	else
	{
		big_integer tmp1 = *this;
		big_integer tmp2 = rhs;
		int p = 0;
		result.sign = false;
		tmp1.sign = false;
		tmp2.sign = false;
		bool flag = true;
		if (tmp1 < tmp2)
		{
			tmp1.sign = this -> sign;
			tmp2.sign = rhs.sign;
			big_integer tmp = tmp1;
			tmp1 = tmp2;
			tmp1 = tmp2;
			tmp2 = tmp;
			flag = false;
		}
		if (flag)
		{
            tmp1.sign = this -> sign;
            tmp2.sign = rhs.sign;
		}
		int n = tmp1.koef.size();
		int m = tmp2.koef.size();
		for (int i = 0; i < m; i++)
		{
			int k = (tmp1.koef[i] - p) - tmp2.koef[i];
			p = 0;
			if (k < 0)
			{
				p = 1;
				k += base;
			}
			result.koef.push_back(k);
		}
		if (n != m)
		{
			if (p)
			{
				int j = m;
				while (tmp1.koef[j] == 0)
				{
					result.koef.push_back(base - 1);
					j++;
				}
				result.koef.push_back(tmp1.koef[j] - 1);
				for (int i = j + 1; i < n; i++)
					result.koef.push_back(tmp1.koef[i]);
			}
			else
			{
				for (int i = m; i < n; i++)
					result.koef.push_back(tmp1.koef[i]);
			}
		}
		if (tmp1.sign == true)
		{
			if (result.sign == true)
				result.sign = false;
			else
				result.sign = true;
		}
		while (result.koef.size() > 1 && result.koef.back() == 0)
		{
			result.koef.pop_back();
		}
		if (result.koef.back() == 0)
		{
			result.sign = false;
		}
		*this = result;
		return *this;
	}
}

big_integer& big_integer::operator-=(big_integer const& rhs)
{
	big_integer tmp2 = rhs;
	if (tmp2.sign == false)
		tmp2.sign = true;
	else
		tmp2.sign = false;
	*this += tmp2;
	return *this;
}

big_integer& mul_short(big_integer a, int b)
{
	int carry = 0;
	for (int i = 0; i < a.koef.size() || carry; i++)
	{
		if (i == a.koef.size())
			a.koef.push_back(0);
		long long cur = carry + a.koef[i] * 1ll * b;
		a.koef[i] = int (cur % base);
		carry = int(cur >> 31);
	}
	while (a.koef.size() > 1 && a.koef.back() == 0)
		a.koef.pop_back();
	return a;
}

big_integer& big_integer::operator*=(big_integer const& rhs)
{
    if(rhs.koef.size() == 1)
	{
		bool c = false;
		if (rhs.sign != this -> sign)
			c = true;
		this -> sign = false;
		*this = mul_short(*this, rhs.koef[0]);
		this -> sign = c;
		return *this;
	}
	big_integer result;
	result.koef.pop_back();
	result.koef.resize(this -> koef.size() + rhs.koef.size());
	for (int i = 0; i < this -> koef.size(); i++)
		for (int j = 0, carry = 0; j < rhs.koef.size() || carry; j++)
		{
			long long cur = result.koef[i + j] + this -> koef[i] * 1ll * (j < rhs.koef.size() ? rhs.koef[j] : 0) + carry;
			result.koef[i + j] = int(cur % base);
			carry = int(cur >> 31);
		}
	while (result.koef.size() > 1 && result.koef.back() == 0)
		result.koef.pop_back();
	char c = false;
	if (this -> sign != rhs.sign)
		c = true;
	*this = result;
	this -> sign = c;
	return *this;
}

big_integer& div_short(big_integer a, int b)
{
	int carry = 0;
	for (int i = a.koef.size() - 1; i >= 0; i--)
	{
		long long cur = a.koef[i] + carry * 1ll * base;
		a.koef[i] = int (cur / b);
		carry = int (cur % b);
	}
	while (a.koef.size() > 1 && a.koef.back() == 0)
		a.koef.pop_back();
	return a;
}

big_integer& big_integer::operator/=(big_integer const& rhs)
{
	big_integer temp1 = *this;
	big_integer temp2 = rhs;
	temp1.sign = false;
	temp2.sign = false;
	if (temp2 > temp1)
	{
		*this = 0;
		return *this;
	}
	if(rhs.koef.size() == 1)
	{
		bool c = false;
		if (rhs.sign != this -> sign)
			c = true;
		this -> sign = false;
		*this = div_short(*this, rhs.koef[0]);
		this -> sign = c;
		return *this;
	}
	else
	{
		big_integer divisor = rhs;
		int mult = base / (divisor.koef.back() + 1);
		*this *= mult;
		divisor *= mult;
		big_integer c;
		bool neg = false;
		if (this -> sign != rhs.sign)
			neg = true;
		this -> sign = false;
		divisor.sign = false;
		long long m = this -> koef.size() - divisor.koef.size();
		c.koef.resize(m + 1);
		big_integer d;
		d.koef.resize(m + 1);
		d.koef[m] = 1;
		big_integer tmp = divisor << (m * 31);// * d
		if (*this >= tmp)
		{
    		c.koef[m] = 1;
    		*this -= tmp;
    	}
    	else
    	{
    		c.koef.pop_back();
    		//c.koef[m] = 0;
    	}
    	d.koef[m] = 0;
    	d.koef.pop_back();
    	for (int j = m - 1; j >= 0; j--)
    	{
            long long tmp1 = ((long long) this -> koef[j + divisor.koef.size()]) << 31;
    		int q = ((long long) tmp1 + this -> koef[j + divisor.koef.size() - 1]) / divisor.koef[divisor.koef.size() - 1];
    		c.koef[j] = min(q, int(base - 1));
    		d.koef[j] = 1;
    		tmp = divisor << (j * 31);//* d;
    		big_integer temp = c.koef[j] * tmp;
    		*this -= temp;
			while (*this < 0)
			{
				c.koef[j]--;
				*this += tmp;
			}
    		if (*this == 0)
    			break;
    		d.koef.pop_back();
    	}
    	while (c.koef.size() > 1 && c.koef.back() == 0)
		{
			c.koef.pop_back();
		}
		if(neg)
			c.sign = true;
		else
    		c.sign = false;
    	return *this = c;
    }
}

big_integer& big_integer::operator%=(big_integer const& rhs)
{
	big_integer t = *this / rhs;
	t *= rhs;
	*this = *this - t;
	return *this;
}

big_integer& big_integer::operator&=(big_integer const& rhs)
{
	big_integer tmp1 = *this;
	big_integer tmp2 = rhs;
	big_integer result;
	result.koef.pop_back();
	vector <int> res;
	int n = this -> koef.size(), m = rhs.koef.size();
	if (this -> sign)
	{
		for (int i = 0; i < n; i++)
		{
			this -> koef[i] = ~this -> koef[i];
		}
		for (int i = n; i < max(n, m); i++)
			this -> koef.push_back(base - 1);
		int i = 0;
		while (this -> koef[i] == base - 1 && i < this -> koef.size())
		{
			this -> koef[i] = 0;
			i++;
		}
		if (i == this -> koef.size())
			this -> koef.push_back(1);
		else
			this -> koef[i]++;
	}
	if (rhs.sign)
	{
		for (int i = 0; i < m; i++)
		{
			tmp2.koef[i] = ~tmp2.koef[i];
		}
		for (int i = m; i < max(n, m); i++)
			tmp2.koef.push_back(base - 1);
		int i = 0;
		while (tmp2.koef[i] == base - 1 && i < tmp2.koef.size())
		{
			tmp2.koef[i] = 0;
			i++;
		}
		if (i == tmp2.koef.size())
			tmp2.koef.push_back(base - 1);
		else
			tmp2.koef[i]++;
	}
	for (int i = 0; i < min(this -> koef.size(), tmp2.koef.size()); i++)
	{
		res.push_back(this -> koef[i] & tmp2.koef[i]);
	}
	if (this -> koef.size() > tmp2.koef.size())
	{
		for (int i = tmp2.koef.size(); i < this -> koef.size(); i++)
			res.push_back(this -> koef[i]);
	}
	if (tmp2.koef.size() > this -> koef.size())
	{
		for (int i = this -> koef.size(); i < tmp2.koef.size(); i++)
			res.push_back(tmp2.koef[i]);
	}
	bool flag = false;
	if (this -> sign &&  rhs.sign)
	{
		flag = true;
		int i = 0;
		while (res[i] == 0)
		{
			res[i] = base - 1;
			i++;
		}
		res[i]--;
		for (int i = 0; i < res.size(); i++)
			res[i] = ~res[i];
	}
	result.sign = flag;
	for (int i = 0; i < res.size(); i++)
		result.koef.push_back(res[i]);
	*this = result;
	return *this;
}

big_integer& big_integer::operator|=(big_integer const& rhs)
{
	big_integer tmp1 = *this;
	big_integer tmp2 = rhs;
	big_integer result;
	result.koef.pop_back();
	vector <int> res;
	int n = this -> koef.size(), m = rhs.koef.size();
	if (this -> sign)
	{
		for (int i = 0; i < n; i++)
		{
			this -> koef[i] = ~this -> koef[i];
		}
		for (int i = n; i < max(n, m); i++)
			this -> koef.push_back(base - 1);
		int i = 0;
		while (this -> koef[i] == base - 1 && i < this -> koef.size())
		{
			this -> koef[i] = 0;
			i++;
		}
		if (i == this -> koef.size())
			this -> koef.push_back(1);
		else
			this -> koef[i]++;
	}
	if (rhs.sign)
	{
		for (int i = 0; i < m; i++)
		{
			tmp2.koef[i] = ~tmp2.koef[i];
		}
		for (int i = m; i < max(n, m); i++)
			tmp2.koef.push_back(base - 1);
		int i = 0;
		while (tmp2.koef[i] == base - 1 && i < tmp2.koef.size())
		{
			tmp2.koef[i] = 0;
			i++;
		}
		if (i == tmp2.koef.size())
			tmp2.koef.push_back(base - 1);
		else
			tmp2.koef[i]++;
	}
	for (int i = 0; i < min(this -> koef.size(), tmp2.koef.size()); i++)
	{
		res.push_back(this -> koef[i] | tmp2.koef[i]);
	}
	if (this -> koef.size() > tmp2.koef.size())
	{
		for (int i = tmp2.koef.size(); i < this -> koef.size(); i++)
			res.push_back(this -> koef[i]);
	}
	if (tmp2.koef.size() > this -> koef.size())
	{
		for (int i = this -> koef.size(); i < tmp2.koef.size(); i++)
			res.push_back(tmp2.koef[i]);
	}
	bool flag = false;
	if (this -> sign ||  rhs.sign)
	{
		flag = true;
		int i = 0;
		while (res[i] == 0)
		{
			res[i] = base - 1;
			i++;
		}
		res[i]--;
		for (int i = 0; i < res.size(); i++)
			res[i] = ~res[i];
	}
	result.sign = flag;
	for (int i = 0; i < res.size(); i++)
		result.koef.push_back(res[i]);
	*this = result;
	return *this;
}

big_integer& big_integer::operator^=(big_integer const& rhs)
{
	big_integer tmp1 = *this;
	big_integer tmp2 = rhs;
	big_integer result;
	result.koef.pop_back();
	vector <int> res;
	int n = this -> koef.size(), m = rhs.koef.size();
	if (this -> sign)
	{
		for (int i = 0; i < n; i++)
		{
			this -> koef[i] = ~this -> koef[i];
		}
		for (int i = n; i < max(n, m); i++)
			this -> koef.push_back(base - 1);
		int i = 0;
		while (this -> koef[i] == base - 1 && i < this -> koef.size())
		{
			this -> koef[i] = 0;
			i++;
		}
		if (i == this -> koef.size())
			this -> koef.push_back(1);
		else
			this -> koef[i]++;
	}
	if (rhs.sign)
	{
		for (int i = 0; i < m; i++)
		{
			tmp2.koef[i] = ~tmp2.koef[i];
		}
		for (int i = m; i < max(n, m); i++)
			tmp2.koef.push_back(base - 1);
		int i = 0;
		while (tmp2.koef[i] == base - 1 && i < tmp2.koef.size())
		{
			tmp2.koef[i] = 0;
			i++;
		}
		if (i == tmp2.koef.size())
			tmp2.koef.push_back(base - 1);
		else
			tmp2.koef[i]++;
	}
	for (int i = 0; i < min(this -> koef.size(), tmp2.koef.size()); i++)
	{
		res.push_back(this -> koef[i] ^ tmp2.koef[i]);
	}
	if (this -> koef.size() > tmp2.koef.size())
	{
		for (int i = tmp2.koef.size(); i < this -> koef.size(); i++)
			res.push_back(this -> koef[i]);
	}
	if (tmp2.koef.size() > this -> koef.size())
	{
		for (int i = this -> koef.size(); i < tmp2.koef.size(); i++)
			res.push_back(tmp2.koef[i]);
	}
	bool flag = false;
	if (this -> sign != rhs.sign)
	{
		flag = true;
		int i = 0;
		while (res[i] == 0)
		{
			res[i] = base - 1;
			i++;
		}
		res[i]--;
		for (int i = 0; i < res.size(); i++)
			res[i] = ~res[i];
	}
	result.sign = flag;
	for (int i = 0; i < res.size(); i++)
		result.koef.push_back(res[i]);
	*this = result;
	return *this;
}

big_integer& big_integer::operator<<=(int rhs)
{
    int k = rhs % 31;
    int p = rhs / 31;
    big_integer st = 1;
    for (int i = 0; i < k; i++)
        st *= 2;
    *this *= st;
    vector <int> res;
    for (int i = this -> koef.size() - 1; i >= 0; i--)
    {
        res.push_back(this -> koef[i]);
    }
    for (int i = 0; i < p; i++)
        res.push_back(0);
    reverse(res.begin(), res.end());
    int t = this -> koef.size();
    for (int i = 0; i < t; i++)
        this -> koef.pop_back();
    for (int i = 0; i < res.size(); i++)
        this -> koef.push_back(res[i]);
    return *this;
}

big_integer& big_integer::operator>>=(int rhs)
{
    bool flag = false;
	if (this -> sign)
	{
		this -> sign = false;
		flag = true;
	}
	int k = rhs % 31;
    int p = rhs / 31;
    big_integer st = 1;
    for (int i = 0; i < k; i++)
        st *= 2;
    *this /= st;
    vector <int> res;
    for (int i = this -> koef.size() - 1; i >= 0; i--)
    {
        res.push_back(this -> koef[i]);
    }
    for (int i = 0; i < p; i++)
        res.pop_back();
    reverse(res.begin(), res.end());
    int t = this -> koef.size();
    for (int i = 0; i < t; i++)
        this -> koef.pop_back();
    for (int i = 0; i < res.size(); i++)
        this -> koef.push_back(res[i]);
    if (flag)
    {
        *this += 1;
        for (int i = 0; i < this -> koef.size(); i++)
        {
            this -> koef[i] = abs(~this -> koef[i]);
        }
        *this -= 1;
        this -> sign = true;
    }
	return *this;
}

big_integer big_integer::operator+() const
{
    return *this;
}

big_integer big_integer::operator-() const
{
    big_integer tmp = *this;
	if (tmp.sign == true)
		tmp.sign = false;
	else
		tmp.sign = true;
	return tmp;
}

big_integer big_integer::operator~() const
{
	big_integer tmp = *this;
	big_integer result;
	result.koef.pop_back();
	if (!this -> sign)
	{
        for (int i = 0; i < this -> koef.size(); i++)
        {
            result.koef.push_back(abs(~this -> koef[i]));
        }
        result.sign = true;
    }
	if (this -> sign)
	{
		for (int i = 0; i < this -> koef.size(); i++)
		{
            result.koef.push_back(~(-this -> koef[i]));
		}
		result.sign = false;
	}
	return result;
}

big_integer& big_integer::operator++()
{
	*this += 1;
    return *this;
}

big_integer big_integer::operator++(int)
{
    big_integer r = *this;
    ++*this;
    return r;
}

big_integer& big_integer::operator--()
{
    *this -= 1;
    return *this;
}

big_integer big_integer::operator--(int)
{
    big_integer r = *this;
    --*this;
    return r;
}

big_integer operator+(big_integer a, big_integer const& b)
{
    return a += b;
}

big_integer operator-(big_integer a, big_integer const& b)
{
    return a -= b;
}

big_integer operator*(big_integer a, big_integer const& b)
{
    return a *= b;
}

big_integer operator/(big_integer a, big_integer const& b)
{
    return a /= b;
}

big_integer operator%(big_integer a, big_integer const& b)
{
    return a %= b;
}

big_integer operator&(big_integer a, big_integer const& b)
{
    return a &= b;
}

big_integer operator|(big_integer a, big_integer const& b)
{
    return a |= b;
}

big_integer operator^(big_integer a, big_integer const& b)
{
    return a ^= b;
}

big_integer operator<<(big_integer a, int b)
{
    return a <<= b;
}

big_integer operator>>(big_integer a, int b)
{
    return a >>= b;
}

bool operator==(big_integer const& a, big_integer const& b)
{
	if (a.koef.size() != b.koef.size())
	{
        return false;
    }
    if (a.sign != b.sign)
    {
        bool flag = true;
        for (int i = 0; i < a.koef.size(); i++)
            if (a.koef[i] != 0 || b.koef[i] != 0)
                flag = false;
        return flag;
    }
    for (int i = 0; i < a.koef.size(); i++)
        if (a.koef[i] != b.koef[i])
            return false;
    return true;
}

bool operator!=(big_integer const& a, big_integer const& b)
{
    return !(a == b);
}

bool operator<(big_integer const& a, big_integer const& b)
{
	if (a.sign == true && b.sign == false)
	{
		if (a != 0 || b != 0)
			return true;
		else
			return false;
	}
	if (a.sign == false && b.sign == true)
		return false;
    if (a.koef.size() < b.koef.size())
        return true;
    if (a.koef.size() > b.koef.size())
        return false;
    for (int i = a.koef.size() - 1; i >= 0; i--)
        if (abs(a.koef[i]) > abs(b.koef[i]))
            return false;
        else if (abs(a.koef[i]) < abs(b.koef[i]))
        	return true;
    return false;
}

bool operator>(big_integer const& a, big_integer const& b)
{
    return (b < a);
}

bool operator<=(big_integer const& a, big_integer const& b)
{
    return !(a > b);
}

bool operator>=(big_integer const& a, big_integer const& b)
{
    return !(a < b);
}

std::string to_string(big_integer const& a)
{
    if (a == 0)
        return "0";
	string s = "";
	if (a.sign == true)
        s += '-';
    big_integer tmp = a;
    tmp.sign = false;
    string t = "";
    while (tmp > 0)
    {
        big_integer k = tmp % 10;
        tmp /= 10;
        t += char(int(k.koef[0]) + int('0'));
    }
    reverse(t.begin(), t.end());
    int q = 0;
    while (t[q] == '0')
        q++;
    t = t.substr(q, t.size() - q);
    s += t;
    return s;
}

int main()
{
    /*big_integer a("-100000000000");
    big_integer b("9");
    big_integer c("5");
    big_integer d("6000000000000000000");

    //a = b;
    

    //cout << a.sign << " " << a.num[0] << " " << endl << b.sign << " " << b.num[0] << endl << endl;
    //cout << (a <= -b) << endl;
    //cout << (a >= -b);
    //cout << c.sign << " " << c.num[0] << endl << endl;
    //cout << abs(c).sign << " " << abs(c).num[0];
    //cerr << endl << endl;
    a = c / b;
    cout << a.sign << " " << a.koef[0] << " " << a.koef[1] << " " << endl << endl;
    //cout << d.sign << " " << d.num[0] << " " << d.num[1] << " " << endl << endl;
    //cout << 5 % 9;

    cout << "!" << to_string(a) << "!";*/

    /*big_integer a;
    big_integer sum = 0;
    for (int i = 1; i <= 2011; i++)
    {
    	a = i * i;
    	if (i % 4 == 2 || i % 4 == 3)
    	{
    		a = -a;
    	}
    	sum += a;
    }
    cout << to_string(sum);*/
    /*big_integer a = 1;
    big_integer sum = 0;
    for (int i = 0; i <= 2011; i++)
    {
    	sum += a;
    	a *= 11;
    }
    cout << to_string(sum);*/

    return 0;
}