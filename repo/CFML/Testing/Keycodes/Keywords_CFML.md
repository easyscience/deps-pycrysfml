# <center>***Keywords on CFML***</center>

## ***Block definitions***
A general block into CFL file have the next structure:
~~~
BlockName_StringIdentification    N
...
...
END_BlockName_StringIdentification
~~~

#### **Examples:**
~~~
  PHASE_PbSO4    1
  ...
  ...
  END_PHASE_PbSO4
~~~

~~~
  PATTERN_XRayMo  1
  ...
  ...
  END_PATTERN_XRayMo
~~~

#### **Exceptions:**
~~~
  COMMANDS
  ...
  ...
  END_COMMANDS
~~~

## ***Pattern Block***

Subblocks allowed within this one

~~~
  BACKGD
  option1 N1
  ...
  ...
  option2 N2
  ...
  ...
  END_BACKGD
~~~
~~~
  EXCLUDE_REGIONS
  ...
  ...
  END_EXCLUDE_REGIONS
~~~

## ***List of Directives***
---
#### **File**

``
FILE POW|HKL|IRF  N_Irf  [N_irf|xye|xys|socabim|old_d1a|...]    Name_of_File
``

Type of file:
**POW**: Powder diffraction data

**HKL**: Integrated intensities data

**IRF**: Instrumental resolution file

Formats of the file
**N_Irf**: Type of IRF file. It is a number





---
##### **Format_File**

``
FORMAT_FILE xye|xys|g41|xrdml|old_d1a|...
``

Define the format of the pattern file

---
##### **Patt_Type**

``
PATT_TYPE Neutrons|X-rays  Pow|Int  [CW|TOF]
``

Define the type of the pattern

---
##### **Zero**

``
Zero 0.1234
``

Define the zero
---
##### **Sycos**

``
Sycos 0.1234
``

---
##### **Sysin**
``
Sysin 0.1234
``


| Directive |Options                   | Observations                        |
|:--------- |:------------------------ |:----------------------------------- |



| Format     | xys| |
|      | socabim| |
|      | xye| |
|      | panalytical| |
| Profile_Function     | |
| Lambda     | |
| Range | Min Max step| |