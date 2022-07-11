# SPDX-License-Identifier: GPL-2.0-or-later

# NOTE: relies on `recomplete` being present.
test:
	emacs -batch -l ../recomplete/recomplete.el -l tests/cycle-at-point-tests.el -f ert-run-tests-batch-and-exit
