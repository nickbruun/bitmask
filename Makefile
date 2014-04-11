all:

test:
	@nosetests
	@pep8 bitmask tests setup.py
	@pyflakes bitmask tests setup.py

pep8:
	@pep8 bitmask tests setup.py

publish:
	@python setup.py sdist upload
