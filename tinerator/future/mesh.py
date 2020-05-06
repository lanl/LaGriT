import numpy as np
from enum import Enum, auto

def test_mesh():
    print('>>> mesh is working')

class ElementType(Enum):
    TRIANGLE = auto()
    QUAD = auto()
    PRISM = auto()
    HEX = auto()

class Mesh:
    def __init__(self, name = 'mesh object', nodes = None, elements = None, etype = None):
        self.name = name
        self.nodes = nodes
        self.elements = elements
        self.element_type = etype
        self.metadata = {}
        self.attributes = {}
    
    def __repr__(self):
        return "Mesh<name: \"{}\", nodes: {}, elements<{}>: {}>".format(
            self.name,
            self.n_nodes,
            self.element_type,
            self.n_elements
        )
    
    def get_attribute(self,name:str):
        try:
            return self.attributes[name]['data']
        except KeyError:
            raise KeyError('Attribute \'%s\' does not exist' % name)
    
    def add_attribute(self,name:str,vector:np.ndarray,attrb_type:str='cell'):
        # TODO: auto-add attribute as cell or node based on array length
        if name in self.attributes:
            raise KeyError('Attribute %s already exists' % name)
        
        if not isinstance(vector,np.ndarray):
            vector = np.array(vector)
        
        attrb_type = attrb_type.lower().strip()

        if attrb_type not in ['cell','node']:
            raise ValueError('`attrb_type` must be either \'cell\' or \'node\'')

        sz = self.n_elements if attrb_type == 'cell' else self.n_nodes
        vector = np.reshape(vector,(sz,))

        self.attributes[name] = { 'type': attrb_type, 'data': vector }
    
    def rm_attribute(self,name:str):
        try:
            del self.attributes[name]
        except KeyError:
            raise KeyError('Attribute \'%s\' does not exist' % name)

    @property
    def material_id(self):
        '''Material ID of mesh'''
        return self.get_attribute('material_id')

    @property
    def x(self):
        '''X vector of mesh nodes'''
        if self.nodes is not None:
            return self.nodes[:,0]
        return None

    @property
    def y(self):
        '''Y vector of mesh nodes'''
        if self.nodes is not None:
            return self.nodes[:,1]
        return None

    @property
    def z(self):
        '''Z vector of mesh nodes'''
        if self.nodes is not None:
            return self.nodes[:,2]
        return None
    
    @property
    def n_nodes(self):
        '''Number of nodes in mesh'''
        if self.nodes is not None:
            return self.nodes.shape[0]
        return None
    
    @property
    def n_elements(self):
        '''Number of elements in mesh'''
        if self.elements is not None:
            return self.elements.shape[0]
        return None

    @property
    def centroid(self):
        '''Mesh centroid'''
        if self.nodes is not None:
            return (np.mean(self.x),np.mean(self.y),np.mean(self.z))
        return None
    
    @property
    def extent(self):
        '''
        Returns the extent of the mesh: 
            [ (x_min, x_max), (y_min, y_max), (z_min, z_max) ]
        '''

        if self.nodes is None:
            return None
        
        ex = []
        for i in range(3):
            vector = self.nodes[:,i]
            ex.append((np.min(vector),np.max(vector)))

        return ex