# Contributing

## How to Contribute

### Contribution Requirements

Pull requests for missing features, bug fixes, and documentation updates are
more than welcome. To contribute, please submit a pull request at

    https://github.com/lanl/LaGriT

To ensure the best chance of acceptance for your pull request, please ensure that:

1. All tests pass
2. Your code is well documented and commented
3. If adding new functionality, that unit tests have been included in the PR
4. That the purpose and scope of your contributions are well explained in the PR

### Need an Issue?

Visit the Issues page of LaGriT and search for tag 'Good First Issue'. 
These are great issues for someone unfamiliar with the codebase to work on.

### Function Docstrings

API documentation is generated using the [pydoc-markdown](https://niklasrosenstein.github.io/pydoc-markdown/) package. Proper formatting
is explained in the [Syntax](https://niklasrosenstein.github.io/pydoc-markdown/)
section of their documentation.

As an example of proper formatting,

```python
def some_function(self,required_param:str,opt_1:str=None,opt_2:int=34):
    '''
    Some function documentation.

    # Arguments
    required_param (str): Some description.

    # Optional Arguments
    opt_1 (str,None): Some description.

    # Raises
    ValueError: If *opt_1* does not end with `.md`.

    # Returns
    The object that is returned.
    '''
```

## Contributor License Agreement (CLA)

If you are an external (non-LANL or LBNL) contributor, then in order to accept
your pull request, we need you to submit a CLA.
This license is for your protection as a Contributor as well as the protection
of LANL and LaGriT users; it does not change your rights to use your own
Contributions for any other purpose.

Sign the CLA here: https://www.clahub.com/agreements/lanl/LaGriT

## License

By contributing to TINerator, you agree that your contributions will be licensed
under the BSD-3 license.