---
title: "Dutch National Flag Problem"
author: Austin
tags: interviews, algorithms
---

Just saw this problem on Glassdoor, given to someone interviewing for an Internship position with Facebook:

> Given an array of randomly arranged lower case letters, uppercase letters, and numbers, sort the array such that all lower case letters come before all uppercase letters, which come before all numbers. The classes of characters do not need to be in order in their respective sections.

So basically, convert ``` B a 2 b 1 A ``` to ``` a b B A 2 1 ```. We rearrange the list so that the lowercase letters are on the left, uppercase letters in the middle, and numbers on the right. You can find more about this problem on wikipedia^[[https://en.wikipedia.org/wiki/Dutch_national_flag_problem](https://en.wikipedia.org/wiki/Dutch_national_flag_problem)]

``` java
void dutchFlag(List<Object> list) {
  int low = 0, equal = 0;
  int high = list.size() - 1;
  while (equal <= high) {
    Object o = list.get(equal);
    if (o instanceof Integer) {
      equal++;
    } else if (o instanceof Character) {
      Character char = (Character)o;
      if (Character.isLowerCase(char)) {
        swap(list, low, equal);
        low++;
        equal++;
      } else if (Character.isUpperCase(char)) {
        swap(list, high, equal);
        high--;
      }
    }
  }
}

void swap(List<Object> list, int i, int j) {
  Object temp = list.get(i);
  list.set(i, list.get(j));
  list.set(j, temp);
}
```
