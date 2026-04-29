Installing PyCrysFML
====================

As PyCrysFML is in its early stages of development, it is not yet accesible from a package installer such as :code:`pip` or :code:`conda`. The steps for using the current version of PyCrysFML are:

1. Download the wheel file from `<https://www.ill.eu/users/support-labs-infrastructure/crysfml>`_.
2. Unzip the wheel file. In *Linux*, do:
    .. code-block:: bash

        tar zxvf name_wheel_file

3. After unzipping, from your Python terminal just type:

    .. code-block:: python

        >>> pip install name_wheel_file

    If the crysfml package was already installed in your machine and you are upgrading the version, then type:

     .. code-block:: python

        >>> pip install name_wheel_file --upgrade
