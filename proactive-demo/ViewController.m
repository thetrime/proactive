//
//  ViewController.m
//  proactive-demo
//
//  Created by Matt Lilley on 13/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "ViewController.h"
#import <Proactive/proactive.h>

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    render(@"http://localhost:8080/react", NULL, @"ios_demo_proactive", self.view);
    NSLog(@"View: %@", self.view);
    // Do any additional setup after loading the view, typically from a nib.
}


- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


@end
