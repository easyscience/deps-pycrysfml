Installing the FullProf Suite
=============================
The different distributions of the FullProf Suite can be downloaded from the `FullProf website <https://www.ill.eu/sites/fullprof/downloads.html>`_. Currently, two versions of the FullProf Suite are available. The classic one, which runs entirely under a :program:`Winteracter` graphical environment, and the modern one, whose graphical environment is based on PySide6, although it still includes some applications built with Winteracter, such as :program:`WinPLOTR` and :program:`EdPCR`.

Classic version
---------------

Windows
^^^^^^^

Download the package installer :program:`Setup_FullProf_Suite_xy.exe` in a temporary directory and run it. Follow the indications of the installer. Avoid directories with spaces or non-ASCII characters. 

Linux
^^^^^

General Mode (all users)
""""""""""""""""""""""""""""""""""""""""""""""

- Activate the super-user mode.
- Create a directory for the FullProf_Suite, for instance :filepath:`/usr/local/bin/FullProf_Suite/`
- Copy the file :filename:`FullProf_Suite_xy_Lin.tgz` (x=month y=year) in the directory created in the previous step.
- Extract the file contents:

  .. code-block:: bash_extension

     tar -xzvf FullProf_Suite_xy_Lin.tgz

- Set up the environment variable :varenv:`FULLPROF`.

Local Mode (single user)
""""""""""""""""""""""""

- Create a directory for the FullProf_Suite, for instance :filepath:`$HOME/FullProf_Suite/`.

    .. code-block:: bash_extension

        cd $HOME
        mkdir FullProf_Suite
        cd FullProf_Suite

- Copy the file :filename:`FullProf_Suite_xy_Linux64.tgz` (x=month y=year) in the directory created in the previous step.
- Extract the file contents:

    .. code-block:: bash_extension

        tar -xzvf FullProf_Suite_xy_Linux64.tgz

- Set up the :varenv:`FULLPROF` environment variable.

    .. code-block:: bash_extension

        source Set_FULLPROF_Envi

In the case of General Mode, the :varenv:`FULLPROF` environment variable must be defined after the installation. The procedure depends on the system shell: Bourne (:program:`sh`, :program:`bash`), Korn (:program:`ksh`) and C (:program:`csh`). We show here the steps for the most common shell, :program:`bash`:

.. code-block:: bash_extension

    FULLPROF=/usr/local/bin/FullProf_Suite
    PATH=$FULLPROF:$PATH
    export FULLPROF

Changes must be effective after opening a new terminal window. To verify that everything is ok, type in your terminal the command:

.. code-block:: bash_extension

    echo $FULLPROF

The shell must return the directory in which you have installed the FullProf Suite. If the :varenv:`FULLPROF` environment variable is well set, the different programs of the FullProf Suite will be available from any directory. Programs can be run in console mode or from the toolbar.

In some cases, the graphical interface may fail to work, showing the following error message: "*libXm.so.3 not available*". The issue can be resolved by installing the libraries :program:`OpenMotif` version 2.3. 

MacOS
^^^^^
Download the file :filename:`FullProf4Mac-vxxx.dmg`, click on it and drag the FullProf4Mac icon to :filepath:`Applications/`.

Requirements
""""""""""""
- macOS High Sierra (10.13) or higher.
- a free X11 server must be present on your computer, either `<www.xquartz.org>`_ (v2.7.9 or higher) or `<ports.macports.org/port/xorg-server/>`_.

Known bugs
""""""""""
- :program:`XQuartz` (at least up to 2.7.11): if :program:`XCode` is not installed on the computer, :program:`XQuartz` will reluctantly ask for *XCode developer tools*. There is no harm in installing those tools to get rid of this error message.
- :program:`XQuartz` (at least up to 2.7.11): :program:`XQuartz` still uses old macOS APIs which makes :program:`X11` based dialogues quite slow. Versions 2.8.1 and higher support both Intel and M1 chips and thus the problem may have disappeared.


Modern version
--------------


Windows
^^^^^^^
Download and unzip the :filename:`FullProf_Suite_PySide6_xy.zip` file. A :filepath:`FullProf_Suite/` folder will be created, containing the executable :program:`tfp` and the subfolder :filepath:`_internal/`. Run the executable by double-clicking on it. The toolbar may take a while to appear, about half a minute. Please be patient.

Linux
^^^^^
Download and extract the contents of the file :filename:`FullProf_Suite_PySide6_xy.tgz`.

.. code-block:: bash_extension

    tar -zxvf FullProf_Suite_PySide6_xy.tgz

A :filepath:`FullProf_Suite/` folder will be created, containing the executable :program:`tfp` and the subfolder :filepath:`_internal/`. Run the executable.

MacOS
^^^^^
We are waiting for a :program:`Winteracter` version that can run on macOS Apple Silicon. As soon as it becomes available, we will distribute the macOS version.